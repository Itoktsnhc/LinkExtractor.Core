namespace LinkExtractor.Core

open System
open System.Collections.Generic
open System.Web
open HtmlAgilityPack
open System.Linq

module Extractor =
    type HtmlLinkNodeType =
        | NodeATag
        | NodeIframe
        | NodeArea

    type ExtractedLink =
        { NodeType: HtmlLinkNodeType
          LinkUrl: string }

    type ILinkValidator =
        abstract Validate: string -> bool

    type ILinkFormatter =
        abstract FormatLink: string -> Uri -> string option

    type ILinkExtractor =
        abstract ExtractLinksFromHtml: HtmlNode -> Uri -> List<ExtractedLink>

    type LinkFormatter() =
        interface ILinkFormatter with
            member this.FormatLink url baseUrl =
                try
                    let proceedLink = url.Replace("\"", "").Trim()

                    let proceedLink =
                        match proceedLink.Contains("#") with
                        | true -> proceedLink.Substring(0, proceedLink.IndexOf('#'))
                        | _ -> proceedLink

                    let uri = Uri(baseUrl, proceedLink)
                    let phpSessionId = "PHPSESSID"

                    let resultUrl =
                        if ((String.IsNullOrEmpty(uri.Query) <> true)
                            && (uri.Query.IndexOf(phpSessionId, StringComparison.OrdinalIgnoreCase)
                                <> -1)) then

                            let queryKeyValuePairs = uri.Query.Split('&')

                            let newQuery =
                                if queryKeyValuePairs.Length > 0 then
                                    let newQueryKeyValuePairs = new List<string>()
                                    queryKeyValuePairs
                                    |> Array.map (fun x -> (x, x.Split('=')))
                                    |> Array.filter (fun (_, y) ->
                                        not
                                            (y.Length = 2
                                             && y.[0].IndexOf(phpSessionId, StringComparison.OrdinalIgnoreCase)
                                                <> -1))
                                    |> Array.iter (fun (x, _) -> newQueryKeyValuePairs.Add(x))
                                    String.Join("&", newQueryKeyValuePairs)
                                else
                                    String.Empty

                            uri.AbsoluteUri.Replace(uri.Query, newQuery)
                        else
                            uri.AbsoluteUri

                    Some resultUrl
                with _ -> None

    type LinkValidator() =
        static let blockedExtensions =
            ".jpg;.png;.gif;.jpeg;.ps;.pdf;.doc;.docx;.ppt;.pptx;.bmp;.exe;.swf;.fla;.dwg;.xls;.xlsx;.wps;.rar;.zip;.mpeg;.tiff;.aif;.bak;.exb;.mp4;.mp3;.apk;.avi;.rmvb;.rm;.asf;.divx;.mpg;.mpe;.wmv;.mkv;.vob"
                .Split(';', StringSplitOptions.RemoveEmptyEntries)
            |> Set.ofArray

        let GetLinkExtension (url: string) =
            let lastIndex = url.LastIndexOf('.')
            match lastIndex = -1 with
            | true -> String.Empty
            | false -> url.Substring(lastIndex, url.Length - lastIndex)

        let validateLinkExtension url =
            let ext = GetLinkExtension url
            match String.IsNullOrEmpty ext with
            | true -> true
            | false -> not (blockedExtensions.Contains ext)

        interface ILinkValidator with
            member this.Validate url =
                match String.IsNullOrEmpty url with
                | true -> false
                | false ->
                    match url.StartsWith("#") with
                    | true -> false
                    | false ->
                        let proceedLink = url.Replace("\"", "").Trim()
                        match proceedLink.StartsWith("mailto:", StringComparison.CurrentCultureIgnoreCase) with
                        | true -> false
                        | false ->
                            match proceedLink.StartsWith("javascript:", StringComparison.CurrentCultureIgnoreCase) with
                            | true -> false
                            | false -> validateLinkExtension url

    type LinkExtractor(linkValidator: ILinkValidator, linkFormatter: ILinkFormatter) =
        let rec RemoveComments (node: HtmlNode) =
            let children = node.ChildNodes
            match children.Count with
            | 0 ->
                match node.NodeType with
                | HtmlNodeType.Comment -> node.Remove()
                | _ -> ()

            | _ ->
                children.ToArray()
                |> Array.iter (fun x -> RemoveComments x)

        //Uri baseUrl, string rawLink, HtmlLinkNodeType nodeType
        let ProcessLink (node: HtmlNode) (baseUrl: Uri) (rawLink: string) (nodeType: HtmlLinkNodeType) =
            match String.IsNullOrEmpty(rawLink) with
            | true -> None
            | false ->
                let rawLink = HttpUtility.UrlDecode(rawLink)
                match linkValidator.Validate rawLink with
                | false -> None
                | true ->
                    let processedLink = linkFormatter.FormatLink rawLink baseUrl
                    match processedLink with
                    | None -> None
                    | Some pLink ->
                        match baseUrl.AbsoluteUri = pLink with
                        | true -> None
                        | false ->
                            Some
                                { NodeType = nodeType
                                  LinkUrl = processedLink.Value }

        interface ILinkExtractor with
            member this.ExtractLinksFromHtml htmlNodeTree baseUrl =
                let links = List<ExtractedLink>()
                RemoveComments htmlNodeTree

                let xPath =
                    match htmlNodeTree.NodeType = HtmlNodeType.Document with
                    | true -> "//a[@href]"
                    | false -> sprintf "%s//a[@href]" htmlNodeTree.XPath

                let htmlNodeCollection = htmlNodeTree.SelectNodes xPath
                match htmlNodeCollection with
                | null -> ()
                | _ ->
                    let subList =
                        htmlNodeCollection
                        |> Seq.map (fun node ->
                            ProcessLink node baseUrl node.Attributes.["href"].Value HtmlLinkNodeType.NodeATag)
                        |> Seq.choose id

                    links.AddRange(subList)

                let xPath =
                    match htmlNodeTree.NodeType = HtmlNodeType.Document with
                    | true -> "//area[@href]"
                    | false -> sprintf "%s//area[@href]" htmlNodeTree.XPath

                let htmlNodeCollection = htmlNodeTree.SelectNodes xPath
                match htmlNodeCollection with
                | null -> ()
                | _ ->
                    let subList =
                        htmlNodeCollection
                        |> Seq.map (fun node ->
                            ProcessLink node baseUrl node.Attributes.["href"].Value HtmlLinkNodeType.NodeArea)
                        |> Seq.choose id

                    links.AddRange(subList)

                let xPath = "//iframe[@src]"
                let htmlNodeCollection = htmlNodeTree.SelectNodes xPath
                match htmlNodeCollection with
                | null -> ()
                | _ ->
                    let subList =
                        htmlNodeCollection
                        |> Seq.map (fun node ->
                            ProcessLink node baseUrl node.Attributes.["src"].Value HtmlLinkNodeType.NodeIframe)
                        |> Seq.choose id

                    links.AddRange(subList)
                links

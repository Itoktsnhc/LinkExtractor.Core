// Learn more about F# at http://fsharp.org

open System
open System.IO
open System.Net.Sockets
open HtmlAgilityPack
open LinkExtractor.Core.Extractor


let source = File.ReadAllText("source.html")

let validator = LinkValidator()
let formatter = LinkFormatter()

let extractor =
    LinkExtractor(validator, formatter) :> ILinkExtractor

let doc = HtmlDocument()
let html = doc.LoadHtml(source)

let links =
    extractor.ExtractLinksFromHtml doc.DocumentNode (Uri("http://newpaper.dahe.cn/hnrb/html/2019-09/16/node_1.htm"))

links |> Array.ofSeq |> printfn "%A"
links.Count|> printfn "%A"

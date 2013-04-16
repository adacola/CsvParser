/// パーザコンビネータを使用したCSVパーザを定義するモジュール
module Wasabi.Csv.ParserCombinator

open FParsec.Primitives
open FParsec.CharParsers

/// CSVをパーズします。
let parseCsv input =
    /// 行区切り
    let rowSeparator = skipNewline <?> "行区切り"
    /// 列区切り
    let columnSeparator = skipChar ',' <?> "列区切り"
    /// クォーテーション文字
    let quote = skipChar '"' <?> "クォーテーション文字"
    /// 2つのクォーテーション文字
    let doubleQuote = quote >>. quote >>% '"' |> attempt <?> "2つのクォーテーション文字"

    /// エスケープ文字
    let escaped = rowSeparator <|> columnSeparator <|> quote <?> "エスケープ文字"
    /// エスケープ文字ではない文字
    let notEscapedChar = notFollowedBy escaped >>. anyChar <?> "エスケープ文字ではない文字"
    /// クォーテーション文字ではない文字
    let notQuoteChar = notFollowedBy quote >>. anyChar <?> "クォーテーション文字ではない文字"

    /// エスケープ文字を含む文字列。クォーテーション文字は2つのクォーテーション文字で表す。
    let escapedText = manyChars (doubleQuote <|> notQuoteChar) |> between quote quote |> attempt
    /// エスケープ文字を含まない文字列
    let notEscapedText = manyChars notEscapedChar |> attempt
    /// データのセル
    let cell = escapedText <|> notEscapedText
    /// データの行
    let row = sepBy cell columnSeparator
    /// CSV全体
    let csv = manyTill (row .>> (rowSeparator <|> eof)) eof

    run csv input |> function Success(result, _, _) -> result | Failure(msg, _, _) -> failwith msg

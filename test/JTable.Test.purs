module JTable.Test where

import Data.Argonaut
import Data.Argonaut.JCursor
import Data.Either
import Data.StrMap

import Test.StrongCheck

import Debug.Trace
import Debug.Foreign

import JTable

sampleJson = """
{
  "userId": 8927524,
  "profile": {
    "name":   "Mary Jane",
    "age":    29,
    "gender": "female"
  },
  "comments": [{
      "id":       "F2372BAC",
      "text":     "I concur.",
      "replyTo":  [9817361, "F8ACD164F"],
      "time":     "2015-02-03"
    }, {
      "id":       "GH732AFC",
      "replyTo":  [9654726, "A44124F"],
      "time":     "2015-03-01"
  }]
}
""" :: String

-- ([Tuple (JField "userId"   (JCursorTop))                                              (8927524),

--   Tuple (JField "profile"  (JField "name"   (JCursorTop)))                              ("Mary Jane"),
--   Tuple (JField "profile"  (JField "age"    (JCursorTop)))                              (29),
--   Tuple (JField "profile"  (JField "gender" (JCursorTop)))                              ("female"),

--   Tuple (JField "comments" (JIndex 0      (JField "id"      (JCursorTop))))             ("F2372BAC"),
--   Tuple (JField "comments" (JIndex 0      (JField "text"    (JCursorTop))))             ("I concur."),
--   Tuple (JField "comments" (JIndex 0      (JField "replyTo" (JIndex 0 (JCursorTop)))))  (9817361),
--   Tuple (JField "comments" (JIndex 0      (JField "replyTo" (JIndex 1 (JCursorTop)))))  ("F8ACD164F"),
--   Tuple (JField "comments" (JIndex 0      (JField "time"    (JCursorTop))))             ("2015-02-03"),

--   Tuple (JField "comments" (JIndex 1      (JField "id"      (JCursorTop))))             ("GH732AFC"),
--   Tuple (JField "comments" (JIndex 1      (JField "replyTo" (JIndex 0 (JCursorTop)))))  (9654726),
--   Tuple (JField "comments" (JIndex 1      (JField "replyTo" (JIndex 1 (JCursorTop)))))  ("A44124F"),
--   Tuple (JField "comments" (JIndex 1      (JField "time"    (JCursorTop))))             ("2015-03-01")])

-- <table>
--   <thead>
--     <tr>
--       <th></th>
--       <th colspan="3">profile</th>
--       <th colspan="5">comments</th>
--     </tr>
--     <tr>
--       <th>userId</th>
--       <th>name</th>
--       <th>age</th>
--       <th>gender</th>
--       <th>id</th>
--       <th>text</th>
--       <th colspan="2">replyTo</th>
--       <th>time</th>
--     </tr>
--   </thead>
--   <tbody>
--     <tr>
--       <td>8927524</td>
--       <td>Mary Jane</td>
--       <td>29</td>
--       <td>female</td>
--       <td>F2372BAC</td>
--       <td>I concur.</td>
--       <td>9817361</td>
--       <td>F8ACD164F</td>
--       <td>2015-02-03</td>
--     </tr>
--     <tr>
--       <td></td>
--       <td></td>
--       <td></td>
--       <td></td>
--       <td>GH732AFC</td>
--       <td></td>
--       <td>9654726</td>
--       <td>A44124F</td>
--       <td>2015-03-01</td>
--     </tr>
--   </tbody>
-- </table>

-- |----------|----------------------------|------------------------------------------------------------|
-- |          |           profile          |                           comments                         |
-- |----------|-----------|-----|----------|----------|--------------|---------------------|------------|
-- |  userId  |    name   | age |  gender  |    id    |     text     |       replyTo       |    time    |
-- |----------|-----------|-----|----------|----------|--------------|---------------------|------------|
-- |  8927524 | Mary Jane |  29 |  female  | F2372BAC | I concur.    | 9817361 | F8ACD164F | 2015-02-03 |
-- |          |           |     |          |----------|--------------|---------------------|------------|
-- |          |           |     |          | GH732AFC |              | 9654726 | A44124F   | 2015-03-01 |
-- |----------|-----------|-----|----------|------------------------------------------------------------|



-- ([Tuple ([0].userId) (8927524),
--   Tuple ([0].profile.name) ("Mary Jane"),
--   Tuple ([0].profile.age) (29),
--   Tuple ([0].profile.gender) ("female"),
--   Tuple ([0].comments[0].id) ("F2372BAC"),
--   Tuple ([0].comments[0].text) ("I concur."),
--   Tuple ([0].comments[0].replyTo[0]) (9817361),
--   Tuple ([0].comments[0].replyTo[1]) ("F8ACD164F"),
--   Tuple ([0].comments[0].time) ("2015-02-03"),
--   Tuple ([0].comments[1].id) ("GH732AFC"),
--   Tuple ([0].comments[1].replyTo[0]) (9654726),
--   Tuple ([0].comments[1].replyTo[1]) ("A44124F"),
--   Tuple ([0].comments[1].time) ("2015-03-01")])

primmms = toPrims <$> jsonParser sampleJson

checkEq :: forall a. (Eq a) => a -> Boolean
checkEq x = (x == x)
     && not (x /= x)

section = trace <<< (++) "\n" <<< flip (++) "\n"





init = print "compilation successful"



-- <table>
--   <thead>
--     <tr>
--       <th>time</th>
--       <th>comments</th>
--       <th>replyTo</th>
--       <th>comments</th>
--       <th>replyTo</th>
--       <th>comments</th>
--       <th>id</th>
--       <th>comments</th>
--       <th>time</th>
--       <th>comments</th>
--       <th>replyTo</th>
--       <th>comments</th>
--       <th>replyTo</th>
--       <th>comments</th>
--       <th>text</th>
--       <th>comments</th>
--       <th>id</th>
--       <th>comments</th>
--       <th>gender</th>
--       <th>profile</th>
--       <th>age</th>
--       <th>profile</th>
--       <th>name</th>
--       <th>profile</th>
--       <th>userId</th>
--     </tr>
--     <tr/>
--   </thead>
--   <tbody>
--     <tr>
--       <td>"2015-03-01"</td>
--       <td>"A44124F"</td>
--       <td>9654726</td>
--       <td>"GH732AFC"</td>
--       <td>"2015-02-03"</td>
--       <td>"F8ACD164F"</td>
--       <td>9817361</td>
--       <td>"I concur."</td>
--       <td>"F2372BAC"</td>
--       <td>"female"</td>
--       <td>29</td>
--       <td>"Mary Jane"</td>
--       <td>8927524</td>
--     </tr>
--     <tr/>
--   </tbody>
-- </table>
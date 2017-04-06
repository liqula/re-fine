{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}


module Refine.Frontend.Test.Samples where

import Data.Text (replace)
import Data.Time.Format (parseTimeOrError, defaultTimeLocale)

import Refine.Prelude (Timestamp(..))
import Refine.Common.Types


sampleTitle :: Title
sampleTitle = Title "Gesellschaftsvertrag Genossenschaft zur universellen Erhabenheit GmbH & Ko KG Ltd. AG, Inc."

sampleAbstract :: Abstract
sampleAbstract = Abstract "Auch gibt es niemanden, der den Schmerz an sich liebt, sucht oder wünscht, nur, weil er Schmerz ist, es sei denn, es kommt zu zufälligen Umständen, in denen Mühen und Schmerz ihm große Freude bereiten können. Um ein triviales Beispiel zu nehmen, wer von uns unterzieht sich je anstrengender körperlicher Betätigung, außer um Vorteile daraus zu ziehen? Aber wer hat irgend ein Recht, einen Menschen zu tadeln, der die Entscheidung trifft, eine Freude zu genießen, die keine unangenehmen Folgen hat, oder einen, der Schmerz vermeidet, welcher keine daraus resultierende Freude nach sich zieht!\n\nWeit hinten, hinter den Wortbergen, fern der Länder Vokalien und Konsonantien leben die Blindtexte. Abgeschieden wohnen sie in Buchstabhausen an der Küste des Semantik, eines großen Sprachozeans. Ein kleines Bächlein namens Duden fließt durch ihren Ort und versorgt sie mit den nötigen Regelialien. Es ist ein paradiesmatisches Land, in dem einem gebratene Satzteile in den Mund fliegen. Nicht einmal von der allmächtigen Interpunktion werden die Blindtexte beherrscht – ein geradezu unorthographisches Leben. Eines Tages aber beschloß eine kleine Zeile Blindtext, ihr Name war Lorem Ipsum, hinaus zu gehen in die weite Grammatik. Der große Oxmox riet ihr davon ab, da es dort wimmele von bösen Kommata, wilden Fragezeichen und hinterhältigen Semikoli, doch das Blindtextchen ließ sich nicht beirren. Es packte seine sieben Versalien, schob sich sein Initial in den Gürtel und machte sich auf den Weg."

sampleText :: VDocVersion 'HTMLRaw
sampleText = vdocVersionFromST
  . replace "\\" ""
  . replace "<strong" " <strong"
  . replace "<a" " <a"
  $ "<p data-uid=\\\"1\\\">Donec\npede\njusto,\nfringilla\nvel,\naliquet\nnec,\nvulputate\neget,\narcu.\nIn\nenim\njusto,\nrhoncus\nut,\nimperdiet\na,\nvenenatis\nvitae,\njusto.\nNullam\ndictum\nfelis\neu\npede\nmollis\npretium.\nInteger\ntincidunt.\nCras\ndapibus.\nVivamus\nelementum\nsemper\nnisi.\nAenean\nvulputate\neleifend\ntellus.\nAenean\nleo\nligula,\nporttitor\neu,\nconsequat\nvitae,\neleifend\nac,\nenim.</p><!-- headings --><h1 data-uid=\\\"2\\\">Heading\n1</h1><h2 data-uid=\\\"3\\\">Heading\n2</h2><h3 data-uid=\\\"4\\\">Heading\n3</h3><h4 data-uid=\\\"5\\\">Heading\n4</h4><h5 data-uid=\\\"6\\\">Heading\n5</h5><h6 data-uid=\\\"7\\\">Heading\n6</h6><!-- copy text --><p data-uid=\\\"8\\\">Lorem\nipsum\ndolor\nsit\namet,<strong data-uid=\\\"47\\\">consectetuer\nadipiscing\nelit</strong>.\nAenean\ncommodo\nligula\neget\ndolor.\nAenean\nmassa.\nCum\nsociis\nnatoque\npenatibus\net\nmagnis\ndis\nparturient\nmontes,\nnascetur\nridiculus\nmus.\nDonec\nquam\nfelis,<a data-uid=\\\"48\\\" href=\\\"#\\\">ultricies\nnec</a>,\npellentesque\neu,\npretium\nquis,\nsem.\nNulla\nconsequat\nmassa\nquis\nenim.<br data-uid=\\\"49\\\">Donec\npede\njusto,\nfringilla\nvel,\naliquet\nnec,\nvulputate\neget,\narcu.\nIn\nenim\njusto,\nrhoncus\nut,\nimperdiet\na,\nvenenatis\nvitae,\njusto.\nNullam\ndictum\nfelis\neu\npede\nmollis\npretium.\nInteger\ntincidunt.\nCras\ndapibus.\nVivamus\nelementum\nsemper\nnisi.\nAenean\nvulputate\neleifend\ntellus.\nAenean\nleo\nligula,\nporttitor\neu,\nconsequat\nvitae,\neleifend\nac,\nenim.</p><p data-uid=\\\"9\\\">Aliquam\nlorem\nante,\ndapibus\nin,\nviverra\nquis,\nfeugiat\na,\ntellus.\nPhasellus\nviverra\nnulla\nut\nmetus\nvarius\nlaoreet.\nQuisque\nrutrum.\nAenean\nimperdiet.\nEtiam\nultricies\nnisi\nvel\naugue.\nCurabitur\nullamcorper\nultricies\nnisi.\nNam\neget\ndui.\nEtiam\nrhoncus.\nMaecenas\ntempus,\ntellus\neget\ncondimentum\nrhoncus,\nsem\nquam\nsemper\nlibero,\nsit\namet\nadipiscing\nsem\nneque\nsed\nipsum.</p><!-- headings in text --><h1 data-uid=\\\"10\\\">Heading\n1</h1><p data-uid=\\\"50\\\">Lorem\nipsum\ndolor\nsit\namet,<strong data-uid=\\\"51\\\">consectetuer\nadipiscing\nelit</strong>.\nAenean\ncommodo\nligula\neget\ndolor.\nAenean\nmassa.\nCum\nsociis\nnatoque\npenatibus\net\nmagnis\ndis\nparturient\nmontes,\nnascetur\nridiculus\nmus.\nDonec\nquam\nfelis,<a data-uid=\\\"52\\\" href=\\\"#\\\">ultricies\nnec</a>,\npellentesque\neu,\npretium\nquis,\nsem.\nNulla\nconsequat\nmassa\nquis\nenim.\nDonec\npede\njusto,\nfringilla\nvel,\naliquet\nnec,\nvulputate\neget,\narcu.</p><h2 data-uid=\\\"11\\\">Heading\n2</h2><p data-uid=\\\"12\\\">Lorem\nipsum\ndolor\nsit\namet,<strong data-uid=\\\"53\\\">consectetuer\nadipiscing\nelit</strong>.\nAenean\ncommodo\nligula\neget\ndolor.\nAenean\nmassa.\nCum\nsociis\nnatoque\npenatibus\net\nmagnis\ndis\nparturient\nmontes,\nnascetur\nridiculus\nmus.\nDonec\nquam\nfelis,<a data-uid=\\\"54\\\" href=\\\"#\\\">ultricies\nnec</a>,\npellentesque\neu,\npretium\nquis,\nsem.\nNulla\nconsequat\nmassa\nquis\nenim.\nDonec\npede\njusto,\nfringilla\nvel,\naliquet\nnec,\nvulputate\neget,\narcu.</p><h3 data-uid=\\\"13\\\">Heading\n3</h3><p data-uid=\\\"55\\\">Lorem\nipsum\ndolor\nsit\namet,<strong data-uid=\\\"56\\\">consectetuer\nadipiscing\nelit</strong>.\nAenean\ncommodo\nligula\neget\ndolor.\nAenean\nmassa.\nCum\nsociis\nnatoque\npenatibus\net\nmagnis\ndis\nparturient\nmontes,\nnascetur\nridiculus\nmus.\nDonec\nquam\nfelis,<a data-uid=\\\"57\\\" href=\\\"#\\\">ultricies\nnec</a>,\npellentesque\neu,\npretium\nquis,\nsem.\nNulla\nconsequat\nmassa\nquis\nenim.\nDonec\npede\njusto,\nfringilla\nvel,\naliquet\nnec,\nvulputate\neget,\narcu.</p><h4 data-uid=\\\"14\\\">Heading\n4</h4><p data-uid=\\\"15\\\">Lorem\nipsum\ndolor\nsit\namet,<strong data-uid=\\\"58\\\">consectetuer\nadipiscing\nelit</strong>.\nAenean\ncommodo\nligula\neget\ndolor.\nAenean\nmassa.\nCum\nsociis\nnatoque\npenatibus\net\nmagnis\ndis\nparturient\nmontes,\nnascetur\nridiculus\nmus.\nDonec\nquam\nfelis,<a data-uid=\\\"59\\\" href=\\\"#\\\">ultricies\nnec</a>,\npellentesque\neu,\npretium\nquis,\nsem.\nNulla\nconsequat\nmassa\nquis\nenim.\nDonec\npede\njusto,\nfringilla\nvel,\naliquet\nnec,\nvulputate\neget,\narcu.</p><h5 data-uid=\\\"16\\\">Heading\n5</h5><p data-uid=\\\"17\\\">Lorem\nipsum\ndolor\nsit\namet,<strong data-uid=\\\"60\\\">consectetuer\nadipiscing\nelit</strong>.\nAenean\ncommodo\nligula\neget\ndolor.\nAenean\nmassa.\nCum\nsociis\nnatoque\npenatibus\net\nmagnis\ndis\nparturient\nmontes,\nnascetur\nridiculus\nmus.\nDonec\nquam\nfelis,<a data-uid=\\\"61\\\" href=\\\"#\\\">ultricies\nnec</a>,\npellentesque\neu,\npretium\nquis,\nsem.\nNulla\nconsequat\nmassa\nquis\nenim.\nDonec\npede\njusto,\nfringilla\nvel,\naliquet\nnec,\nvulputate\neget,\narcu.</p><h6 data-uid=\\\"18\\\">Heading\n6</h6><p data-uid=\\\"19\\\">Lorem\nipsum\ndolor\nsit\namet,<strong data-uid=\\\"62\\\">consectetuer\nadipiscing\nelit</strong>.\nAenean\ncommodo\nligula\neget\ndolor.\nAenean\nmassa.\nCum\nsociis\nnatoque\npenatibus\net\nmagnis\ndis\nparturient\nmontes,\nnascetur\nridiculus\nmus.\nDonec\nquam\nfelis,<a data-uid=\\\"63\\\" href=\\\"#\\\">ultricies\nnec</a>,\npellentesque\neu,\npretium\nquis,\nsem.\nNulla\nconsequat\nmassa\nquis\nenim.\nDonec\npede\njusto,\nfringilla\nvel,\naliquet\nnec,\nvulputate\neget,\narcu.</p><!-- simple link --><p data-uid=\\\"20\\\"><a data-uid=\\\"21\\\" href=\\\"#\\\">Textlink</a></p><!-- table --><table data-uid=\\\"22\\\"><tr data-uid=\\\"23\\\"><th data-uid=\\\"24\\\">Table\nHeading\nA</th><th data-uid=\\\"25\\\">Table\nHeading\nB</th></tr><tr data-uid=\\\"26\\\"><td data-uid=\\\"27\\\">Table\nData\nA</td><td data-uid=\\\"28\\\">Table\nData\nB</td></tr></table><!-- unordered list --><ul data-uid=\\\"29\\\"><li data-uid=\\\"30\\\">Eintrag\nA</li><li data-uid=\\\"31\\\">Eintrag\nB</li><li data-uid=\\\"32\\\">Eintrag\nC</li></ul><!-- ordered list --><ol data-uid=\\\"33\\\"><li data-uid=\\\"34\\\">Eintrag\n1</li><li data-uid=\\\"35\\\">Eintrag\n2</li><li data-uid=\\\"36\\\">Eintrag\n3</li><li data-uid=\\\"37\\\">Eintrag\n4</li><li data-uid=\\\"38\\\">Eintrag\n5</li><li data-uid=\\\"39\\\">Eintrag\n6</li><li data-uid=\\\"40\\\">Eintrag\n7</li><li data-uid=\\\"41\\\">Eintrag\n8</li><li data-uid=\\\"42\\\">Eintrag\n9</li><li data-uid=\\\"43\\\">Eintrag\n10</li><li data-uid=\\\"45\\\">Eintrag\n11</li><li data-uid=\\\"46\\\">Eintrag\n12</li></ol>"

sampleID :: ID a
sampleID = ID 1

sampleTime :: Timestamp
sampleTime = Timestamp $ parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" "2017-04-06 08:44:40 CEST" 

sampleUserInfo :: UserInfo
sampleUserInfo = Anonymous

sampleMeta :: Meta
sampleMeta = Meta sampleUserInfo sampleTime sampleUserInfo sampleTime

sampleMetaID :: MetaID a
sampleMetaID = MetaID sampleID sampleMeta

module rycodExample
open FSharp.Data

/// Example XMLto train FSharp type provider.  NB: not a valid rycod example, has been simplified for brevity
type ThumperRycod = FSharp.Data.XmlProvider<"""<?xml version="1.0" encoding="ISO-8859-1" standalone="yes"?>
<ryseComponentRequest d1p1:schemaLocation="http://thumper.amyris.local" xmlns:d1p1="http://www.w3.org/2001/XMLSchema-instance" xmlns="http://thumper.amyris.local">
<rabitSpec name="gYNG2$C227C.hb" breed="X" upstreamLink="0" downstreamLink="2" direction="FWD" id="R.31277" creator="platt">
<dnaElementSpec speciesVariant="CENPK2">
<upstreamPrimerSpec>
<tail>gacggcacggccacgcgtttaaaccgcc</tail>
<body>ggacaacataaccaatagaagatg</body>
</upstreamPrimerSpec>
<downstreamPrimerSpec>
<tail>aggtccgccggcgttggacgagcg</tail>
<body>ggaatcatatcttagtttgattcacaag</body>
</downstreamPrimerSpec>
<dnaSequence>
GGACAACATAACCAATAGAAGATGGATCCAAGTTTAGTTTTAGAGCAAACGATACAAGATGTGTCCAACCTCCCATCAGAATTTCGTTACCTCTTAGAGGAGATCGGTTCAAATGATTTGAAGCTCATCGAAGAAAAAAAGAAATACGAGCAAAAAGAATCACAAATACACAAATTTATAAGACAGCAAGGCTCAATACCGAAACATCCACAGGAAGATGGGCTTGACAAAGAAATAAAAGAATCACTTTTGAAATGTCAGTCTTTGCAAAGAGAAAAATGCGTTCTGGCGAACACTGCCTTGTTTCTAATTGCTAGACACTTGAATAAGTTGGAAAAAAACATCGCTTTATTGGAGGAAGATGGTGTTCTAGCCCCCGTGGAAGAAGATGGAGACATGGATAGCGCTGCTGAAGCCTCTAGAGAAAGTTCAGTTGTGAGTAACAGTAGCGTGAAAAAGAGAAGAGCTGCATCAAGCTCAGGATCCGTTCCACCCACTTTGAAAAAGAAAAAAACTAGTCGAACATCTAAACTGCAAAATGAAATTGACGTTTCTTCAAGAGAAAAGTCTGTTACTCCAGTGAGCCCAAGCATTGAAAACAAGATTGCAAGAACAAAAGAATTCAAAAACAGTAGAAATGGTAAAGGCCAAAACGGTTCCCCTGAAAACGAAGAGGAAGATAAGACCCTCTATTGTTTTTGCCAAAGAGTTTCGTTTGGAGAAATGGTTGCATGTGATGGACCCAACTGTAAATATGAATGGTTTCATTATGATTGTGTAAATTTAAAAGAACCTCCGAAAGGAACATGGTACTGTCCCGAATGTAAAATTGAGATGGAAAAAAACAAACTGAAAAGAAAACGTAACTGAACAAAAATACCTTTTTGAGACTGAATGACCTCATTTACACTAATAATATTCCATAAGCCAGAAACGGTGATTGCTATTCTGACAATGTGCTTTTCGAAAAGGAGAAGTAATTTCGAGACATCAGCTAGCAAGGACATATACGATTGACATTTTTTGAATGTGCTTTCAAATACTTGTGAATCAAACTAAGATATGATTCC
</dnaSequence>
<quickChangeSpec>
<forwardPrimerSpec>
<tail>gaagaggaagataagaccctctattgtttttgc</tail>
<body>caaagagtttcgtttggagaaat</body>
</forwardPrimerSpec>
<reversePrimerSpec>
<tail>gcaaaaacaatagagggtcttatcttcctcttc</tail>
<body>gttttcaggggaaccgttttg</body>
</reversePrimerSpec>
</quickChangeSpec>
</dnaElementSpec>
</rabitSpec>
 
 <rabitSpec direction="FWD" name="foo6" upstreamLink="2" breed="PsGsT" downstreamLink="3" creator="parry" id="R.31519">
<dnaElementSpec speciesVariant="CENPK2">
<upstreamPrimerSpec>
<tail>cgctcgtccaacgccggcggacct</tail>
<body>gacggtagcaacaagaatatagc</body>
</upstreamPrimerSpec>
<downstreamPrimerSpec>
<tail>tgacctgtctttccagtcat</tail>
<body>ttttgagggaatattcaactgttttt</body>
</downstreamPrimerSpec>
<dnaSequence>
CGGCCCGACGCGACGCGCCAAAAAATGAAAAAAGAAGCAGCGACTCATTTTTATGGAAGGACAAAGTGCTGCGAAGTCATACGCTTCCAATTTCATTGTTGTTTATTGGACATACTCTGT
</dnaSequence>
</dnaElementSpec>
<dnaElementSpec speciesVariant="Other">
<upstreamPrimerSpec>
<tail>agttgaatattccctcaaaa</tail>
<body>atgactggaaagacaggtcacat</body>
</upstreamPrimerSpec>
<downstreamPrimerSpec>
<tail>acggccggccaagcacgcggggat</tail>
<body>agttatgacaattacaacaacagaat</body>
</downstreamPrimerSpec>
<dnaSequence>
CGGCCCGACGCGACGCGCCAAAAAATGAAAAAAGAAGCAGCGACTCATTTTTATGGAAGGACAAAGTGCTGCGAAGTCATACGCTTCCAATTTCATTGTTGTTTATTGGACATACTCTGTTAGCTTTATTACCATCCACCCTTTTTCTAGAATAGTGTAAAAGTTTCTTTTTATGTTTATCGTATTCATAAAATGCTTCACGAACACCGTCATTGATCAAATAGGTCTATAATATTAATATACATTTATATAATCTACGGTATTTATATCATCAAAAAAAAGTAGTTTTTTATTTATTTT
</dnaSequence>
</dnaElementSpec>
</rabitSpec>

  <rabitSpec direction="REV" name="URA3 2of2" upstreamLink="4" breed="M" downstreamLink="9" creator="parry" id="R.2077">
  </rabitSpec>
  <rabitSpec direction="FWD" name="bar" upstreamLink="A" breed="GsT" downstreamLink="3" creator="parry" id="R.2114">
    <dnaElementSpec speciesVariant="Other">
      <upstreamPrimerSpec>
        <tail>ACCTCCCGCGACCTCCAAAATCGAACTACCTTCACA</tail>
        <body>atgaagcaaagaattggtgcttac</body>
      </upstreamPrimerSpec>
      <downstreamPrimerSpec>
        <tail>ACGGCCGGCCAAGCACGCGGGGAT</tail>
        <body>atgtcggacagtatgcagtgtatataagtatttc</body>
      </downstreamPrimerSpec>
      <dnaSequence>AGAGTTATAACGAAATGTCAAATAATTCTACGGTAATATAACTTATCAGCGGCGTATACT</dnaSequence>
    </dnaElementSpec>
  </rabitSpec>
  <rabitSpec direction="FWD" name="FOO1" upstreamLink="B" breed="GsT" downstreamLink="4" creator="parry" id="R.2234">
    <dnaElementSpec speciesVariant="CENPK2">
      <upstreamPrimerSpec>
        <tail>ACGCACGCACACTCCCGACAGACAACTAGCTTGATA</tail>
        <body>atgtctcagaacgtttacattgta</body>
      </upstreamPrimerSpec>
      <downstreamPrimerSpec>
        <tail>ATCGGCGCTCGCGGCCTGCAGGTT</tail>
        <body>cggccagcttgccaaggcg</body>
      </downstreamPrimerSpec>
      <dnaSequence>GTACCGCTAATTTAGCAGGGCAGTATTATTGTAGTTTGATATGTACGGCTAACTGAACCT</dnaSequence>
    </dnaElementSpec>
  </rabitSpec>
  <rabitSpec direction="FWD" name="BAR6" upstreamLink="C" breed="PP" downstreamLink="B" creator="parry" id="R.2246">
    <dnaElementSpec speciesVariant="CENPK2">
      <upstreamPrimerSpec>
        <tail>TGTAAAGTTAGTTGGTTGCGCGACTTCGGGTGGGGT</tail>
        <body>aagtatagaggtatattaacaatt</body>
      </upstreamPrimerSpec>
      <downstreamPrimerSpec>
        <tail>TATCAAGCTAGTTGTCTGTCGGGAGTGTGCGTGCGT</tail>
        <body>tactttttttttggatggacgcaa</body>
      </downstreamPrimerSpec>
      <dnaSequence>ATCCTATCCGCAATATCCTAAAAGCATAACTAATGCTTCTTTAATCTTGTATGTGACACT</dnaSequence>
    </dnaElementSpec>
  </rabitSpec>
  <rabitSpec direction="FWD" name="US_GOO" upstreamLink="0" breed="U" downstreamLink="2" creator="parry" id="R.2260">
    <dnaElementSpec speciesVariant="CENPK2">
      <upstreamPrimerSpec>
        <tail>GACGGCACGGCCACGCGTTTAAACCGCC</tail>
        <body>cggtctctcactaggtgttcacattatgt</body>
      </upstreamPrimerSpec>
      <downstreamPrimerSpec>
        <tail>AGGTCCGCCGGCGTTGGACGAGCG</tail>
        <body>tttaagcgctttttataatattgttatttctcttta</body>
      </downstreamPrimerSpec>
      <dnaSequence>ACTTGTAGCTCCACTACCCTGATCTGCAATCTTGTTCTTAGAAGTGACGCATATTCTATA</dnaSequence>
    </dnaElementSpec>
  </rabitSpec>
  <rabitSpec direction="FWD" name="yay" upstreamLink="2" breed="P" downstreamLink="A" creator="parry" id="R.2293">
    <dnaElementSpec speciesVariant="CENPK2">
      <upstreamPrimerSpec>
        <tail>cgctcgtccaacgccggcggacct</tail>
        <body>tcacatgtagggaccgaattgtttacaagttc</body>
      </upstreamPrimerSpec>
      <downstreamPrimerSpec>
        <tail>tgtgaaggtagttcgattttggaggtcgcgggaggt</tail>
        <body>ttttttatcatgttgatgctctgc</body>
      </downstreamPrimerSpec>
      <dnaSequence>AGGGTCTGCAAATCTGTGAACTCTCGGCGATTATCTTGGTGCAATTACGTAATTTTAGCC</dnaSequence>
    </dnaElementSpec>
  </rabitSpec>
  <rabitSpec direction="REV" name="DS_GOO" upstreamLink="0" breed="D" downstreamLink="2" creator="parry" id="R.2331">
    <dnaElementSpec speciesVariant="CENPK2">
      <upstreamPrimerSpec>
        <tail>GACGGCACGGCCACGCGTTTAAACCGCC</tail>
        <body>tgtgataattattcatagaaatattacagagg</body>
      </upstreamPrimerSpec>
      <downstreamPrimerSpec>
        <tail>AGGTCCGCCGGCGTTGGACGAGCG</tail>
        <body>ataaactaatgattttaaatcgttaaaaaaatatgc</body>
      </downstreamPrimerSpec>
      <dnaSequence>ataaactaatgattttaaatcgttaaaaaaatatgcgaattctgtggatcgaacacaggacctccagataacttgaccgaagttttttcttcagtctggcgctctcccaactgagctaaatccgcttactatttgttatcagttcccttcatatctacatagaataggttaagtattttattagttgccagaagaactactgatagttgggaatatttggtgaataatgaagattgggtgaataatttgataattttgagattcaattgttaatcaatgttacaatattatgtatacagagtatactagaagttctcttcggagatcttgaagttcacaaaagggaatcgatatttctacataatattatcattacttcttccccatcttatatttgtcattcattattgattatgatcaatgcaataatgattggtagttgccaaacatttaatacgatcctctgtaatatttctatgaataattatcaca</dnaSequence>
    </dnaElementSpec>
  </rabitSpec>
  <rabitSpec direction="FWD" name="some string" upstreamLink="C" breed="PP" downstreamLink="B" creator="parry" id="R.2340">
    <dnaElementSpec speciesVariant="CENPK2">
      <upstreamPrimerSpec>
        <tail>TGTAAAGTTAGTTGGTTGCGCGACTTCGGGTGGGGT</tail>
        <body>gcagccaccagccgcttctc</body>
      </upstreamPrimerSpec>
      <downstreamPrimerSpec>
        <tail>TATCAAGCTAGTTGTCTGTCGGGAGTGTGCGTGCGT</tail>
        <body>ttgagactactcttggggacttta</body>
      </downstreamPrimerSpec>
      <dnaSequence>gcagccaccagccgcttctcgagcaaagtgtagatcccattaggactcatcattcatctaattttgctatgttagctgcaactttctattttaatagaaccttctggaaatttcacccggcgcggcacccgaggaactggacagcgtgtcgaaaaagttgcttttttatataaaggacacgaaaagggttctctggaagatataaatatggctatgtaattctaaagattaacgtgttactgttttacttttttaaagtccccaagagtagtctcaa</dnaSequence>
    </dnaElementSpec>
  </rabitSpec>
  <rabitSpec direction="FWD" name="ABC1" upstreamLink="A" breed="GsT" downstreamLink="3" creator="jimmy" id="R.2520">
    <dnaElementSpec speciesVariant="CENPK2">
      <upstreamPrimerSpec>
        <tail>ACCTCCCGCGACCTCCAAAATCGAACTACCTTCACA</tail>
        <body>atgatctctgatgaacagctgaac</body>
      </upstreamPrimerSpec>
      <downstreamPrimerSpec>
        <tail>ACGGCCGGCCAAGCACGCGGGGAT</tail>
        <body>aaggagatgtcttgtttatctcgagtgtatttatc</body>
      </downstreamPrimerSpec>
      <dnaSequence>ATGATCTCTGATGAACAGCTGAACTCCTTGGCCATCACCTTCGGTATTGTGATGATGACTTTAATTGTCATTTACCATGCTGTTGACTCCACCATGTCTCCTAAGAACTAAagtggttacatttggatagcgaacttcagtttttattatacgtcgctttgtgtttctgcaaaacaaaacaaaactacaactacaaagtactgcgttggatttcaatcagtgctacattaatttttctccctttttacttcgttttacgcacaaaaatatacagataaatacactcgagataaacaagacatctcctt</dnaSequence>
    </dnaElementSpec>
  </rabitSpec>
  <rabitSpec direction="REV" name="YPT32" upstreamLink="3" breed="GsT" downstreamLink="C" creator="parry" id="R.2564">
    <dnaElementSpec speciesVariant="CENPK2">
      <upstreamPrimerSpec>
        <tail>ATCCCCGCGTGCTTGGCCGGCCGT</tail>
        <body>ggcgtcaggaaaggtctatttagaaaatgtatg</body>
      </upstreamPrimerSpec>
      <downstreamPrimerSpec>
        <tail>ACCCCACCCGAAGTCGCGCAACCAACTAACTTTACA</tail>
        <body>atgagcaacgaagattacggatac</body>
      </downstreamPrimerSpec>
      <dnaSequence>TAGCTTTATTACCATCCACCCTTTTTCTAGAATAGTGTAAAAGTTTCTTTTTATGTTTAT</dnaSequence>
    </dnaElementSpec>
  </rabitSpec>
  <stitchSpec id="S.16" creator="jeeves">
    <rabitRef rabitSpec="R.2331"/>
    <rabitRef rabitSpec="R.2293"/>
    <rabitRef rabitSpec="R.2520"/>
    <rabitRef rabitSpec="R.2564"/>
    <rabitRef rabitSpec="R.2246"/>
    <rabitRef rabitSpec="R.2234"/>
    <rabitRef rabitSpec="R.2077"/>
  </stitchSpec>
  <stitchSpec id="S.553" creator="jeeves">
    <rabitRef rabitSpec="R.2260"/>
    <rabitRef rabitSpec="R.2293"/>
    <rabitRef rabitSpec="R.2114"/>
    <rabitRef rabitSpec="R.2219"/>
    <rabitRef rabitSpec="R.2340"/>
    <rabitRef rabitSpec="R.2210"/>
    <rabitRef rabitSpec="R.2074"/>
  </stitchSpec>
  <megastitchSpec id="MS.666" creator="parry">
    <stitchRef stitchSpec="S.553"/>
    <stitchRef stitchSpec="S.16"/>
  </megastitchSpec>
  <megastitchSpec id="MS.666" creator="parry">
    <stitchRef stitchSpec="S.553"/>
    <stitchRef stitchSpec="S.16"/>
  </megastitchSpec>
</ryseComponentRequest>
""">

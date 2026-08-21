-- Seed: 15178391200803990420,16188444798499499427

library ieee;
use ieee.std_logic_1164.all;

entity sq is
  port (sted : buffer time; t : in std_logic_vector(4 to 1); xtuegjic : out time; hkkisurbu : in integer);
end sq;

architecture q of sq is
  
begin
  -- Single-driven assignments
  xtuegjic <= 8#0_3_1_1_3.1_5_1_2# us;
  sted <= 1 sec;
end q;

entity btsskzj is
  port (sae : inout boolean; janybh : linkage boolean_vector(2 downto 3); nchxr : inout time);
end btsskzj;

library ieee;
use ieee.std_logic_1164.all;

architecture k of btsskzj is
  signal ghlj : time;
  signal crgfkb : std_logic_vector(4 to 1);
  signal ss : time;
  signal wza : integer;
  signal flgvpw : std_logic_vector(4 to 1);
  signal fq : time;
begin
  txisopnc : entity work.sq
    port map (sted => fq, t => flgvpw, xtuegjic => nchxr, hkkisurbu => wza);
  mqhalq : entity work.sq
    port map (sted => ss, t => crgfkb, xtuegjic => ghlj, hkkisurbu => wza);
  
  -- Single-driven assignments
  sae <= FALSE;
  wza <= 16#7#;
  
  -- Multi-driven assignments
  flgvpw <= (others => '0');
  flgvpw <= flgvpw;
end k;

library ieee;
use ieee.std_logic_1164.all;

entity cursq is
  port (ef : in std_logic_vector(0 downto 2));
end cursq;

architecture ybgjndilg of cursq is
  signal lgfpiikrfl : integer;
  signal gksvymhyq : time;
  signal dcnvrlblwn : time;
  signal hizqxzc : integer;
  signal e : time;
  signal ljxbxhsqjl : time;
begin
  kvnk : entity work.sq
    port map (sted => ljxbxhsqjl, t => ef, xtuegjic => e, hkkisurbu => hizqxzc);
  gjzoc : entity work.sq
    port map (sted => dcnvrlblwn, t => ef, xtuegjic => gksvymhyq, hkkisurbu => lgfpiikrfl);
  
  -- Single-driven assignments
  lgfpiikrfl <= 2#11#;
  hizqxzc <= hizqxzc;
end ybgjndilg;

library ieee;
use ieee.std_logic_1164.all;

entity rlp is
  port (d : out std_logic; eh : buffer std_logic_vector(3 downto 4));
end rlp;

library ieee;
use ieee.std_logic_1164.all;

architecture assrq of rlp is
  signal cnppldkyo : time;
  signal rwqdc : boolean_vector(2 downto 3);
  signal es : boolean;
  signal xeocgoaff : integer;
  signal folglaautq : time;
  signal vjlcpyrg : std_logic_vector(4 to 1);
  signal yynbbjdnd : time;
begin
  thgxptmxfy : entity work.sq
    port map (sted => yynbbjdnd, t => vjlcpyrg, xtuegjic => folglaautq, hkkisurbu => xeocgoaff);
  by : entity work.btsskzj
    port map (sae => es, janybh => rwqdc, nchxr => cnppldkyo);
  suyywkcxvz : entity work.cursq
    port map (ef => eh);
  
  -- Single-driven assignments
  xeocgoaff <= xeocgoaff;
  
  -- Multi-driven assignments
  eh <= eh;
  eh <= eh;
  vjlcpyrg <= eh;
  d <= '0';
end assrq;



-- Seed after: 5770237500817505832,16188444798499499427

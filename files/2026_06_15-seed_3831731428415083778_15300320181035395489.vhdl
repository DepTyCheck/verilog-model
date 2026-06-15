-- Seed: 3831731428415083778,15300320181035395489

library ieee;
use ieee.std_logic_1164.all;

entity ngztuzlid is
  port (jgbbubyq : linkage std_logic_vector(3 to 0); pfabemqdp : linkage boolean; nwl : inout bit_vector(4 to 3); ptcrqqt : in integer);
end ngztuzlid;

architecture kal of ngztuzlid is
  
begin
  
end kal;

library ieee;
use ieee.std_logic_1164.all;

entity rhssew is
  port (u : inout string(1 to 4); wjwtdm : buffer real; hyrcbnnywn : linkage std_logic_vector(1 to 1); ecoocqqzb : linkage std_logic_vector(4 to 1));
end rhssew;

architecture jtg of rhssew is
  
begin
  -- Single-driven assignments
  wjwtdm <= 16#0.E5511#;
end jtg;

entity wnflxfm is
  port (kvfswanhg : buffer boolean; zqkr : buffer integer);
end wnflxfm;

library ieee;
use ieee.std_logic_1164.all;

architecture vfe of wnflxfm is
  signal fehpbhh : std_logic_vector(4 to 1);
  signal mvfkzad : std_logic_vector(1 to 1);
  signal tpmz : real;
  signal ifzoswir : string(1 to 4);
  signal sqna : real;
  signal ceixo : string(1 to 4);
  signal gmx : std_logic_vector(4 to 1);
  signal jxesvghvx : std_logic_vector(1 to 1);
  signal upbxzay : real;
  signal asbxbc : string(1 to 4);
  signal fwllxjgop : integer;
  signal wncwqhmfm : bit_vector(4 to 3);
  signal muhfevy : std_logic_vector(4 to 1);
begin
  ivxzm : entity work.ngztuzlid
    port map (jgbbubyq => muhfevy, pfabemqdp => kvfswanhg, nwl => wncwqhmfm, ptcrqqt => fwllxjgop);
  pwyex : entity work.rhssew
    port map (u => asbxbc, wjwtdm => upbxzay, hyrcbnnywn => jxesvghvx, ecoocqqzb => gmx);
  yknsnlmsss : entity work.rhssew
    port map (u => ceixo, wjwtdm => sqna, hyrcbnnywn => jxesvghvx, ecoocqqzb => muhfevy);
  zyuqjefcd : entity work.rhssew
    port map (u => ifzoswir, wjwtdm => tpmz, hyrcbnnywn => mvfkzad, ecoocqqzb => fehpbhh);
  
  -- Single-driven assignments
  zqkr <= 2#10#;
  
  -- Multi-driven assignments
  gmx <= (others => '0');
end vfe;

entity jzbpfmcnk is
  port (ivztmqhw : buffer integer_vector(2 downto 3));
end jzbpfmcnk;

library ieee;
use ieee.std_logic_1164.all;

architecture v of jzbpfmcnk is
  signal ktquplf : boolean;
  signal utfmtsl : integer;
  signal aaqtrt : bit_vector(4 to 3);
  signal pbcldoxpf : boolean;
  signal rjhgexch : integer;
  signal vapp : bit_vector(4 to 3);
  signal jodgqy : boolean;
  signal xqt : std_logic_vector(3 to 0);
begin
  xeoerpr : entity work.ngztuzlid
    port map (jgbbubyq => xqt, pfabemqdp => jodgqy, nwl => vapp, ptcrqqt => rjhgexch);
  umesdghsow : entity work.ngztuzlid
    port map (jgbbubyq => xqt, pfabemqdp => pbcldoxpf, nwl => aaqtrt, ptcrqqt => utfmtsl);
  kxzpidao : entity work.wnflxfm
    port map (kvfswanhg => ktquplf, zqkr => utfmtsl);
  
  -- Single-driven assignments
  ivztmqhw <= (others => 0);
  rjhgexch <= 8#7_7#;
  
  -- Multi-driven assignments
  xqt <= (others => '0');
end v;



-- Seed after: 4924885946689178794,15300320181035395489

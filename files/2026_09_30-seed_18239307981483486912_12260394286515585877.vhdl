-- Seed: 18239307981483486912,12260394286515585877

entity hxl is
  port (bqb : out time_vector(0 to 0); fjgjhfb : out real; buvhd : inout real; bckkah : buffer time);
end hxl;

architecture rwxryr of hxl is
  
begin
  -- Single-driven assignments
  fjgjhfb <= 8#7_6.0#;
  bckkah <= bckkah;
  bqb <= (others => 2#0001# ms);
  buvhd <= 1.2_1;
end rwxryr;

library ieee;
use ieee.std_logic_1164.all;

entity gohelxhvq is
  port (dgc : out std_logic_vector(4 to 0); iplorbv : linkage std_logic);
end gohelxhvq;

architecture qd of gohelxhvq is
  signal npafju : time;
  signal exmr : real;
  signal eif : real;
  signal kgrwtxq : time_vector(0 to 0);
  signal wirp : time;
  signal xwtgq : real;
  signal fckbag : real;
  signal ecusmpvk : time_vector(0 to 0);
  signal hyinrbzdf : time;
  signal akiiabbvid : real;
  signal dftp : real;
  signal cdvhjwlsd : time_vector(0 to 0);
  signal cxun : time;
  signal ddnurvtekd : real;
  signal ox : real;
  signal ott : time_vector(0 to 0);
begin
  wumvbjo : entity work.hxl
    port map (bqb => ott, fjgjhfb => ox, buvhd => ddnurvtekd, bckkah => cxun);
  vojqzmpz : entity work.hxl
    port map (bqb => cdvhjwlsd, fjgjhfb => dftp, buvhd => akiiabbvid, bckkah => hyinrbzdf);
  ts : entity work.hxl
    port map (bqb => ecusmpvk, fjgjhfb => fckbag, buvhd => xwtgq, bckkah => wirp);
  ljlmhiea : entity work.hxl
    port map (bqb => kgrwtxq, fjgjhfb => eif, buvhd => exmr, bckkah => npafju);
  
  -- Multi-driven assignments
  dgc <= (others => '0');
  dgc <= dgc;
  dgc <= (others => '0');
  dgc <= dgc;
end qd;

library ieee;
use ieee.std_logic_1164.all;

entity wbvdybrclx is
  port (gwiwqpt : linkage std_logic_vector(3 downto 2); xaeblgwxj : buffer time; fo : in integer_vector(1 to 4); dnqay : linkage std_logic);
end wbvdybrclx;

library ieee;
use ieee.std_logic_1164.all;

architecture bfyxtujj of wbvdybrclx is
  signal rmk : real;
  signal qdlqmqjsaz : real;
  signal ie : time_vector(0 to 0);
  signal vqablz : std_logic;
  signal tuu : std_logic_vector(4 to 0);
  signal zekuegwk : time;
  signal g : real;
  signal ufjn : real;
  signal n : time_vector(0 to 0);
begin
  xgrwmohe : entity work.hxl
    port map (bqb => n, fjgjhfb => ufjn, buvhd => g, bckkah => zekuegwk);
  ipfpv : entity work.gohelxhvq
    port map (dgc => tuu, iplorbv => vqablz);
  himeb : entity work.hxl
    port map (bqb => ie, fjgjhfb => qdlqmqjsaz, buvhd => rmk, bckkah => xaeblgwxj);
end bfyxtujj;

entity eni is
  port (dp : in integer; fv : out time; spdqdcq : inout severity_level);
end eni;

library ieee;
use ieee.std_logic_1164.all;

architecture fax of eni is
  signal gptffwtj : integer_vector(1 to 4);
  signal pwdwjayd : time;
  signal m : integer_vector(1 to 4);
  signal wsjvzjl : std_logic_vector(3 downto 2);
  signal ft : std_logic;
  signal sgwwwixu : std_logic_vector(4 to 0);
  signal emellkajsp : std_logic;
  signal jmzlh : std_logic_vector(4 to 0);
begin
  jyykv : entity work.gohelxhvq
    port map (dgc => jmzlh, iplorbv => emellkajsp);
  qm : entity work.gohelxhvq
    port map (dgc => sgwwwixu, iplorbv => ft);
  fclbq : entity work.wbvdybrclx
    port map (gwiwqpt => wsjvzjl, xaeblgwxj => fv, fo => m, dnqay => emellkajsp);
  wddyksb : entity work.wbvdybrclx
    port map (gwiwqpt => wsjvzjl, xaeblgwxj => pwdwjayd, fo => gptffwtj, dnqay => emellkajsp);
  
  -- Single-driven assignments
  spdqdcq <= spdqdcq;
  m <= (8#5#, 2324, 16#02F96#, 8#3_0_3_2_4#);
  
  -- Multi-driven assignments
  ft <= 'H';
  jmzlh <= jmzlh;
end fax;



-- Seed after: 2963486061364596057,12260394286515585877

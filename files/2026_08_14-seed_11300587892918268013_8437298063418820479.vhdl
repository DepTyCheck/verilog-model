-- Seed: 11300587892918268013,8437298063418820479

entity mvvfu is
  port (cn : linkage boolean; wardtpfw : buffer integer);
end mvvfu;

architecture ahukud of mvvfu is
  
begin
  -- Single-driven assignments
  wardtpfw <= 8#0#;
end ahukud;

entity mgj is
  port (jfeepsn : buffer character; wfxl : out integer; sglxmy : in integer);
end mgj;

architecture lr of mgj is
  signal maej : boolean;
  signal cge : integer;
  signal ccnsja : boolean;
begin
  ngsvjsaqo : entity work.mvvfu
    port map (cn => ccnsja, wardtpfw => cge);
  ltk : entity work.mvvfu
    port map (cn => maej, wardtpfw => wfxl);
end lr;

entity kaqajkpjs is
  port (idjhredjg : out integer; gsdila : inout bit_vector(1 downto 2); itl : out time);
end kaqajkpjs;

architecture hfgr of kaqajkpjs is
  signal ox : integer;
  signal zrquxrigu : boolean;
begin
  pwh : entity work.mvvfu
    port map (cn => zrquxrigu, wardtpfw => ox);
  
  -- Single-driven assignments
  itl <= 8#6_2_3_0_1.1_6_3# us;
  idjhredjg <= 240;
  gsdila <= (others => '0');
end hfgr;

library ieee;
use ieee.std_logic_1164.all;

entity ydlmhb is
  port (cicrzufcxg : linkage std_logic; yzti : buffer integer; shnegnlybq : out integer);
end ydlmhb;

architecture tlfajxc of ydlmhb is
  signal x : boolean;
  signal pdahbo : integer;
  signal jr : integer;
  signal unfau : character;
begin
  lufdk : entity work.mgj
    port map (jfeepsn => unfau, wfxl => jr, sglxmy => pdahbo);
  pqrhsw : entity work.mvvfu
    port map (cn => x, wardtpfw => yzti);
  
  -- Single-driven assignments
  shnegnlybq <= 04;
  pdahbo <= shnegnlybq;
end tlfajxc;



-- Seed after: 12881635131365850864,8437298063418820479

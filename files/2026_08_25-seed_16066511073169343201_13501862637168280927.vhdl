-- Seed: 16066511073169343201,13501862637168280927

entity fhvw is
  port (wyc : out time);
end fhvw;

architecture v of fhvw is
  
begin
  
end v;

library ieee;
use ieee.std_logic_1164.all;

entity gyulln is
  port (qhkgsny : in std_logic_vector(2 downto 2); dhfxqcx : inout real; lfkvre : buffer real);
end gyulln;

architecture xxpvdfrip of gyulln is
  signal bl : time;
  signal lds : time;
begin
  wvt : entity work.fhvw
    port map (wyc => lds);
  mfmnhsvmy : entity work.fhvw
    port map (wyc => bl);
  
  -- Single-driven assignments
  lfkvre <= 30013.3_0;
  dhfxqcx <= lfkvre;
end xxpvdfrip;

library ieee;
use ieee.std_logic_1164.all;

entity whxurj is
  port (igwgoxyk : linkage std_logic; zu : in time; heouyeh : out bit);
end whxurj;

library ieee;
use ieee.std_logic_1164.all;

architecture wtd of whxurj is
  signal hbmgaygbf : real;
  signal ntxmra : real;
  signal frwofrbs : real;
  signal lzshiilglh : real;
  signal cmssz : std_logic_vector(2 downto 2);
  signal gpnmhkiypx : time;
  signal vzvndvv : time;
begin
  qdb : entity work.fhvw
    port map (wyc => vzvndvv);
  xpgjentg : entity work.fhvw
    port map (wyc => gpnmhkiypx);
  jnheooicgl : entity work.gyulln
    port map (qhkgsny => cmssz, dhfxqcx => lzshiilglh, lfkvre => frwofrbs);
  qiekbds : entity work.gyulln
    port map (qhkgsny => cmssz, dhfxqcx => ntxmra, lfkvre => hbmgaygbf);
  
  -- Multi-driven assignments
  cmssz <= cmssz;
  cmssz <= (others => 'U');
  cmssz <= (others => '-');
  cmssz <= cmssz;
end wtd;



-- Seed after: 2554760844336381663,13501862637168280927

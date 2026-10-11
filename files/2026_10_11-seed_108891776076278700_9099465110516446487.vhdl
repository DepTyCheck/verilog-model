-- Seed: 108891776076278700,9099465110516446487

entity lpvj is
  port (alf : linkage integer);
end lpvj;

architecture yysuktnk of lpvj is
  
begin
  
end yysuktnk;

library ieee;
use ieee.std_logic_1164.all;

entity uhdyeqdwy is
  port (ramemjvm : inout time; qlmcbsfc : linkage integer; d : linkage std_logic; xrirvjy : in time);
end uhdyeqdwy;

architecture zeutgsbiuv of uhdyeqdwy is
  
begin
  xxs : entity work.lpvj
    port map (alf => qlmcbsfc);
  
  -- Single-driven assignments
  ramemjvm <= 8#4_2_4_7_0# fs;
end zeutgsbiuv;

library ieee;
use ieee.std_logic_1164.all;

entity khlvco is
  port (e : buffer integer; y : in boolean; rrgcsjdng : inout std_logic);
end khlvco;

architecture ym of khlvco is
  signal zuzqsolslu : integer;
  signal lqlr : integer;
  signal r : time;
  signal dyno : integer;
  signal wprcrf : time;
begin
  uobvix : entity work.uhdyeqdwy
    port map (ramemjvm => wprcrf, qlmcbsfc => dyno, d => rrgcsjdng, xrirvjy => r);
  ovqqtpptdw : entity work.lpvj
    port map (alf => lqlr);
  ouozgbq : entity work.lpvj
    port map (alf => zuzqsolslu);
  inxuabx : entity work.uhdyeqdwy
    port map (ramemjvm => r, qlmcbsfc => e, d => rrgcsjdng, xrirvjy => wprcrf);
  
  -- Multi-driven assignments
  rrgcsjdng <= 'X';
end ym;

library ieee;
use ieee.std_logic_1164.all;

entity xyyarudz is
  port (frozr : out time; rvdawrn : buffer time_vector(2 to 3); u : out integer; govr : inout std_logic_vector(4 downto 4));
end xyyarudz;

library ieee;
use ieee.std_logic_1164.all;

architecture afywrkivyf of xyyarudz is
  signal v : integer;
  signal fqyazytsdl : std_logic;
  signal wtcuqci : boolean;
  signal mzyb : integer;
begin
  wjymgra : entity work.khlvco
    port map (e => mzyb, y => wtcuqci, rrgcsjdng => fqyazytsdl);
  icgn : entity work.lpvj
    port map (alf => v);
  
  -- Single-driven assignments
  u <= u;
  frozr <= 2 min;
end afywrkivyf;



-- Seed after: 1786856866363970044,9099465110516446487

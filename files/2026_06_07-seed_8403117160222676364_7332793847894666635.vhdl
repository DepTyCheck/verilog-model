-- Seed: 8403117160222676364,7332793847894666635

library ieee;
use ieee.std_logic_1164.all;

entity z is
  port (pf : out real_vector(2 to 0); jetkbwmbsn : in std_logic; tubtqq : linkage integer; djawsqg : out boolean);
end z;



architecture oqlbruov of z is
  
begin
  
end oqlbruov;

library ieee;
use ieee.std_logic_1164.all;

entity jnsrfjenc is
  port (acr : out real_vector(1 downto 4); hbfaokp : linkage std_logic_vector(3 to 0));
end jnsrfjenc;

library ieee;
use ieee.std_logic_1164.all;

architecture zhrtafmjjr of jnsrfjenc is
  signal hamlw : boolean;
  signal nnrtnvhs : integer;
  signal n : real_vector(2 to 0);
  signal jbvxuonts : boolean;
  signal xuykt : integer;
  signal dujvb : real_vector(2 to 0);
  signal ab : boolean;
  signal xgmsmdixxu : std_logic;
  signal bmyedmtv : real_vector(2 to 0);
  signal gfkzerq : boolean;
  signal p : integer;
  signal vqi : std_logic;
begin
  ukeyel : entity work.z
    port map (pf => acr, jetkbwmbsn => vqi, tubtqq => p, djawsqg => gfkzerq);
  ebrxv : entity work.z
    port map (pf => bmyedmtv, jetkbwmbsn => xgmsmdixxu, tubtqq => p, djawsqg => ab);
  uxodhfflzj : entity work.z
    port map (pf => dujvb, jetkbwmbsn => vqi, tubtqq => xuykt, djawsqg => jbvxuonts);
  dpvqf : entity work.z
    port map (pf => n, jetkbwmbsn => xgmsmdixxu, tubtqq => nnrtnvhs, djawsqg => hamlw);
end zhrtafmjjr;



entity dvb is
  port (cnxplogxfc : inout bit_vector(4 downto 3));
end dvb;

library ieee;
use ieee.std_logic_1164.all;

architecture xh of dvb is
  signal vi : std_logic_vector(3 to 0);
  signal jplex : real_vector(1 downto 4);
  signal hhcn : std_logic_vector(3 to 0);
  signal xw : real_vector(1 downto 4);
  signal wwiche : boolean;
  signal leyudt : integer;
  signal ftjyhjcth : std_logic;
  signal vgxvifiaz : real_vector(2 to 0);
begin
  p : entity work.z
    port map (pf => vgxvifiaz, jetkbwmbsn => ftjyhjcth, tubtqq => leyudt, djawsqg => wwiche);
  je : entity work.jnsrfjenc
    port map (acr => xw, hbfaokp => hhcn);
  mpdyk : entity work.jnsrfjenc
    port map (acr => jplex, hbfaokp => vi);
end xh;



-- Seed after: 18285195169181392480,7332793847894666635

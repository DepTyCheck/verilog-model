-- Seed: 3395776288652195366,15795020531041709203

library ieee;
use ieee.std_logic_1164.all;

entity ufmescsmn is
  port (bi : out std_logic_vector(2 to 0); jedf : buffer integer; qds : in bit; kteujs : linkage real);
end ufmescsmn;

architecture d of ufmescsmn is
  
begin
  -- Single-driven assignments
  jedf <= jedf;
  
  -- Multi-driven assignments
  bi <= (others => '0');
end d;

entity xahgzj is
  port (olqrvjky : buffer integer; gakrvxde : buffer real; lphu : inout real);
end xahgzj;

library ieee;
use ieee.std_logic_1164.all;

architecture kjk of xahgzj is
  signal jfdtjxnmca : integer;
  signal ymesel : std_logic_vector(2 to 0);
  signal ouhxxpkniw : real;
  signal bzwjb : integer;
  signal ibjio : real;
  signal fhblxsopyg : bit;
  signal hvsdkratg : integer;
  signal zecalfyr : std_logic_vector(2 to 0);
begin
  mupgas : entity work.ufmescsmn
    port map (bi => zecalfyr, jedf => hvsdkratg, qds => fhblxsopyg, kteujs => lphu);
  lepawmh : entity work.ufmescsmn
    port map (bi => zecalfyr, jedf => olqrvjky, qds => fhblxsopyg, kteujs => ibjio);
  zd : entity work.ufmescsmn
    port map (bi => zecalfyr, jedf => bzwjb, qds => fhblxsopyg, kteujs => ouhxxpkniw);
  ag : entity work.ufmescsmn
    port map (bi => ymesel, jedf => jfdtjxnmca, qds => fhblxsopyg, kteujs => gakrvxde);
  
  -- Single-driven assignments
  fhblxsopyg <= '0';
  
  -- Multi-driven assignments
  zecalfyr <= "";
  ymesel <= ymesel;
  ymesel <= zecalfyr;
end kjk;



-- Seed after: 14444359413131978758,15795020531041709203

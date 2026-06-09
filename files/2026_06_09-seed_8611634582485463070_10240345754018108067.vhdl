-- Seed: 8611634582485463070,10240345754018108067

library ieee;
use ieee.std_logic_1164.all;

entity d is
  port (qppcwim : buffer std_logic_vector(3 downto 2); zosmy : inout bit_vector(3 downto 1); lvpoc : in time);
end d;



architecture kpzmu of d is
  
begin
  
end kpzmu;



entity jnfbtsqsa is
  port (qmjxjfb : inout severity_level; orfyftdorc : buffer real; xx : linkage time; txxzf : out integer);
end jnfbtsqsa;

library ieee;
use ieee.std_logic_1164.all;

architecture yox of jnfbtsqsa is
  signal fkqajjomv : bit_vector(3 downto 1);
  signal sizm : std_logic_vector(3 downto 2);
  signal doei : time;
  signal wlhafyxnm : bit_vector(3 downto 1);
  signal cdwv : std_logic_vector(3 downto 2);
begin
  ptirwit : entity work.d
    port map (qppcwim => cdwv, zosmy => wlhafyxnm, lvpoc => doei);
  fuguiflhjc : entity work.d
    port map (qppcwim => sizm, zosmy => fkqajjomv, lvpoc => doei);
end yox;

library ieee;
use ieee.std_logic_1164.all;

entity qxfir is
  port (usihj : inout time; jzxituhhrf : inout integer; stjhnrlg : buffer std_logic; snpbxtkyrp : out time);
end qxfir;

library ieee;
use ieee.std_logic_1164.all;

architecture wpgxd of qxfir is
  signal qkxof : bit_vector(3 downto 1);
  signal vlubzp : time;
  signal cjcovmkns : bit_vector(3 downto 1);
  signal k : std_logic_vector(3 downto 2);
begin
  azueg : entity work.d
    port map (qppcwim => k, zosmy => cjcovmkns, lvpoc => vlubzp);
  wejdzhcjou : entity work.d
    port map (qppcwim => k, zosmy => qkxof, lvpoc => vlubzp);
end wpgxd;



entity dtycc is
  port (jexcrtpacj : buffer time);
end dtycc;

library ieee;
use ieee.std_logic_1164.all;

architecture ub of dtycc is
  signal bjcveeg : time;
  signal ntbgiokhf : bit_vector(3 downto 1);
  signal qjt : std_logic_vector(3 downto 2);
  signal fiigg : bit_vector(3 downto 1);
  signal i : std_logic_vector(3 downto 2);
  signal fabaf : std_logic;
  signal pmhb : integer;
  signal kmeg : time;
  signal prmmtemx : bit_vector(3 downto 1);
  signal nsmbp : std_logic_vector(3 downto 2);
begin
  e : entity work.d
    port map (qppcwim => nsmbp, zosmy => prmmtemx, lvpoc => jexcrtpacj);
  ksmnpqmydu : entity work.qxfir
    port map (usihj => kmeg, jzxituhhrf => pmhb, stjhnrlg => fabaf, snpbxtkyrp => jexcrtpacj);
  ybjsmrmmc : entity work.d
    port map (qppcwim => i, zosmy => fiigg, lvpoc => jexcrtpacj);
  rf : entity work.d
    port map (qppcwim => qjt, zosmy => ntbgiokhf, lvpoc => bjcveeg);
end ub;



-- Seed after: 9334530872127029056,10240345754018108067

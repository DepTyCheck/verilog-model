-- Seed: 5168268093424862404,5415160250146859793



entity modcuwm is
  port (dvzguyi : buffer real_vector(2 downto 1); mwurem : inout real);
end modcuwm;



architecture npwkqdctc of modcuwm is
  
begin
  
end npwkqdctc;



entity afuy is
  port (vcewnuqb : buffer integer; xdnpudpznv : buffer real; lkdak : linkage real_vector(3 downto 1); gchsaaelp : buffer bit);
end afuy;



architecture bfxrflte of afuy is
  signal roona : real;
  signal rvhgosje : real_vector(2 downto 1);
  signal gsn : real_vector(2 downto 1);
begin
  hboyqcf : entity work.modcuwm
    port map (dvzguyi => gsn, mwurem => xdnpudpznv);
  zfm : entity work.modcuwm
    port map (dvzguyi => rvhgosje, mwurem => roona);
end bfxrflte;

library ieee;
use ieee.std_logic_1164.all;

entity ghlobiutdi is
  port (zftbrwb : buffer std_logic; zphr : out std_logic; oozzc : out integer_vector(4 to 1); iahmnjmtu : linkage boolean);
end ghlobiutdi;



architecture az of ghlobiutdi is
  signal v : real;
  signal tz : real_vector(2 downto 1);
  signal fggpekxjt : real;
  signal vfwhdseeo : real_vector(2 downto 1);
  signal xn : bit;
  signal wghfeqapq : real_vector(3 downto 1);
  signal agpkguucr : real;
  signal iaixaik : integer;
  signal ra : bit;
  signal ojz : real_vector(3 downto 1);
  signal aujkbnpnv : real;
  signal mt : integer;
begin
  fmbig : entity work.afuy
    port map (vcewnuqb => mt, xdnpudpznv => aujkbnpnv, lkdak => ojz, gchsaaelp => ra);
  sknwgm : entity work.afuy
    port map (vcewnuqb => iaixaik, xdnpudpznv => agpkguucr, lkdak => wghfeqapq, gchsaaelp => xn);
  pmekrzt : entity work.modcuwm
    port map (dvzguyi => vfwhdseeo, mwurem => fggpekxjt);
  wkta : entity work.modcuwm
    port map (dvzguyi => tz, mwurem => v);
end az;



entity akka is
  port (eamqwbtkbr : in integer);
end akka;

library ieee;
use ieee.std_logic_1164.all;

architecture ez of akka is
  signal jffruhnd : boolean;
  signal wlwciojv : integer_vector(4 to 1);
  signal n : std_logic;
  signal tpkl : std_logic;
  signal rowztshlf : real;
  signal eltsbuwql : real_vector(2 downto 1);
  signal xon : bit;
  signal p : real_vector(3 downto 1);
  signal zva : real;
  signal kpmsxwlkg : integer;
begin
  eg : entity work.afuy
    port map (vcewnuqb => kpmsxwlkg, xdnpudpznv => zva, lkdak => p, gchsaaelp => xon);
  krkymhnmx : entity work.modcuwm
    port map (dvzguyi => eltsbuwql, mwurem => rowztshlf);
  ft : entity work.ghlobiutdi
    port map (zftbrwb => tpkl, zphr => n, oozzc => wlwciojv, iahmnjmtu => jffruhnd);
end ez;



-- Seed after: 9149119370733024332,5415160250146859793

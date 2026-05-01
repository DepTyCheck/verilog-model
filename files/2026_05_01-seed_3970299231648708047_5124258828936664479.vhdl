-- Seed: 3970299231648708047,5124258828936664479



entity dohzkanvpf is
  port (cnbhqzp : linkage real; tep : buffer time; btcadsxmhz : buffer real; dgdoliu : linkage real);
end dohzkanvpf;



architecture u of dohzkanvpf is
  
begin
  
end u;



entity t is
  port (v : linkage bit; otkkcnl : in integer);
end t;



architecture ojpdsuisn of t is
  signal akskyem : time;
  signal kr : real;
  signal ukjhzfcun : time;
  signal pczkwrnzz : real;
begin
  afchod : entity work.dohzkanvpf
    port map (cnbhqzp => pczkwrnzz, tep => ukjhzfcun, btcadsxmhz => pczkwrnzz, dgdoliu => kr);
  iprrmc : entity work.dohzkanvpf
    port map (cnbhqzp => pczkwrnzz, tep => akskyem, btcadsxmhz => kr, dgdoliu => pczkwrnzz);
end ojpdsuisn;



entity ek is
  port (j : out integer; piirym : linkage bit);
end ek;



architecture qnxezqbaqw of ek is
  signal ucpg : real;
  signal qixtl : real;
  signal rzvzjfbsvn : time;
  signal pkdd : real;
  signal aynduhgi : real;
  signal ulbetyctwf : real;
  signal hl : time;
  signal vji : real;
  signal lvntqhvsrg : real;
  signal ubgzfqyl : time;
  signal jcgxpimhe : real;
begin
  bz : entity work.dohzkanvpf
    port map (cnbhqzp => jcgxpimhe, tep => ubgzfqyl, btcadsxmhz => jcgxpimhe, dgdoliu => lvntqhvsrg);
  benz : entity work.dohzkanvpf
    port map (cnbhqzp => vji, tep => hl, btcadsxmhz => ulbetyctwf, dgdoliu => aynduhgi);
  ed : entity work.dohzkanvpf
    port map (cnbhqzp => pkdd, tep => rzvzjfbsvn, btcadsxmhz => qixtl, dgdoliu => ucpg);
end qnxezqbaqw;

library ieee;
use ieee.std_logic_1164.all;

entity fkgbd is
  port (rksz : inout std_logic);
end fkgbd;



architecture pxlkjmbet of fkgbd is
  signal okodmhzvua : real;
  signal ltqbzpky : time;
  signal rlmvctddk : real;
  signal ayqygnvvfb : time;
  signal mqtt : real;
  signal vdavrmi : integer;
  signal zfnol : bit;
begin
  ku : entity work.t
    port map (v => zfnol, otkkcnl => vdavrmi);
  ykmlfst : entity work.dohzkanvpf
    port map (cnbhqzp => mqtt, tep => ayqygnvvfb, btcadsxmhz => rlmvctddk, dgdoliu => rlmvctddk);
  zr : entity work.dohzkanvpf
    port map (cnbhqzp => mqtt, tep => ltqbzpky, btcadsxmhz => okodmhzvua, dgdoliu => mqtt);
end pxlkjmbet;



-- Seed after: 13623109042025188995,5124258828936664479

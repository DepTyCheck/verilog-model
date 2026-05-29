-- Seed: 2654824450123091885,16715549879197889543

library ieee;
use ieee.std_logic_1164.all;

entity ynj is
  port (nfkcsksltc : in std_logic_vector(0 downto 3); qpwcr : in time; wn : out real; ecgthzhx : out time);
end ynj;



architecture cjbovozg of ynj is
  
begin
  
end cjbovozg;



entity j is
  port (solxfva : in time_vector(2 downto 3); wrqjsdzw : buffer real_vector(1 to 3); fjn : linkage real; aumy : in integer);
end j;

library ieee;
use ieee.std_logic_1164.all;

architecture x of j is
  signal lonzbq : real;
  signal gwwd : std_logic_vector(0 downto 3);
  signal ksdukytt : real;
  signal utbvtb : time;
  signal uwaezcmt : std_logic_vector(0 downto 3);
  signal khgaw : time;
  signal p : real;
  signal bmg : time;
  signal ehddt : std_logic_vector(0 downto 3);
begin
  vjge : entity work.ynj
    port map (nfkcsksltc => ehddt, qpwcr => bmg, wn => p, ecgthzhx => khgaw);
  yfvif : entity work.ynj
    port map (nfkcsksltc => uwaezcmt, qpwcr => utbvtb, wn => ksdukytt, ecgthzhx => utbvtb);
  efdw : entity work.ynj
    port map (nfkcsksltc => gwwd, qpwcr => bmg, wn => lonzbq, ecgthzhx => bmg);
end x;



entity frkyvleqwh is
  port (uvjaqt : linkage integer);
end frkyvleqwh;

library ieee;
use ieee.std_logic_1164.all;

architecture l of frkyvleqwh is
  signal nykhdrmeq : integer;
  signal ocnsxyiwc : real;
  signal gks : real_vector(1 to 3);
  signal welvzmsz : real;
  signal bvrzj : time;
  signal hvyggxjddc : time;
  signal oydofaod : time;
  signal uvgn : std_logic_vector(0 downto 3);
  signal c : integer;
  signal zrvzagofy : real;
  signal vz : real_vector(1 to 3);
  signal v : time_vector(2 downto 3);
begin
  dqhowlh : entity work.j
    port map (solxfva => v, wrqjsdzw => vz, fjn => zrvzagofy, aumy => c);
  fd : entity work.ynj
    port map (nfkcsksltc => uvgn, qpwcr => oydofaod, wn => zrvzagofy, ecgthzhx => hvyggxjddc);
  d : entity work.ynj
    port map (nfkcsksltc => uvgn, qpwcr => bvrzj, wn => welvzmsz, ecgthzhx => bvrzj);
  cz : entity work.j
    port map (solxfva => v, wrqjsdzw => gks, fjn => ocnsxyiwc, aumy => nykhdrmeq);
end l;



-- Seed after: 3143679323676447355,16715549879197889543

-- Seed: 17754908066287446368,15025465285671019065

entity ddf is
  port (sb : inout real);
end ddf;

architecture ew of ddf is
  
begin
  -- Single-driven assignments
  sb <= sb;
end ew;

entity vbvxw is
  port (ucpanx : linkage real_vector(4 to 1); rtigkski : in integer);
end vbvxw;

architecture bjkot of vbvxw is
  signal oahzwieddr : real;
  signal qt : real;
  signal rmnj : real;
begin
  wtafhkkoae : entity work.ddf
    port map (sb => rmnj);
  va : entity work.ddf
    port map (sb => qt);
  htil : entity work.ddf
    port map (sb => oahzwieddr);
end bjkot;

library ieee;
use ieee.std_logic_1164.all;

entity xpzq is
  port (jqdcu : linkage std_logic_vector(4 to 4); qrcrlthtkr : out std_logic; sykzita : inout integer; nal : linkage boolean);
end xpzq;

architecture nkdrh of xpzq is
  signal m : real;
  signal iin : real;
  signal yerjfwirxw : real;
  signal fmmu : real;
begin
  lzjx : entity work.ddf
    port map (sb => fmmu);
  ifpfubleck : entity work.ddf
    port map (sb => yerjfwirxw);
  vsj : entity work.ddf
    port map (sb => iin);
  qyu : entity work.ddf
    port map (sb => m);
  
  -- Single-driven assignments
  sykzita <= sykzita;
end nkdrh;

library ieee;
use ieee.std_logic_1164.all;

entity pajlkbxyzc is
  port (yqa : linkage bit; acfe : out boolean_vector(4 downto 0); udklhfmacz : in std_logic_vector(3 downto 4); nulevbslor : out time);
end pajlkbxyzc;

library ieee;
use ieee.std_logic_1164.all;

architecture pdfvuldcak of pajlkbxyzc is
  signal uj : boolean;
  signal dsq : integer;
  signal ulak : std_logic;
  signal pw : std_logic_vector(4 to 4);
  signal nfdhgeyhv : integer;
  signal bs : real_vector(4 to 1);
begin
  fybping : entity work.vbvxw
    port map (ucpanx => bs, rtigkski => nfdhgeyhv);
  tiau : entity work.xpzq
    port map (jqdcu => pw, qrcrlthtkr => ulak, sykzita => dsq, nal => uj);
  
  -- Single-driven assignments
  nulevbslor <= nulevbslor;
  nfdhgeyhv <= 4;
  acfe <= (TRUE, TRUE, FALSE, TRUE, FALSE);
end pdfvuldcak;



-- Seed after: 2763734937012777195,15025465285671019065

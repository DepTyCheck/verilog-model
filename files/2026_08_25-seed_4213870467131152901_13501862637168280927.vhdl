-- Seed: 4213870467131152901,13501862637168280927

library ieee;
use ieee.std_logic_1164.all;

entity jxgfzo is
  port (gchtniqjtp : in std_logic; pjypo : in time; layfuoem : out time_vector(0 to 4); nsyfsy : inout time);
end jxgfzo;

architecture hqpsq of jxgfzo is
  
begin
  -- Single-driven assignments
  nsyfsy <= 16#730F# ms;
  layfuoem <= (16#0_E_D_F.F_0_1_B# ms, 16#778.D_7# us, 2#11# ms, 1 hr, 2_0_0_1_0.1 ms);
end hqpsq;

entity wzm is
  port (jkoin : in integer; uyyp : linkage real; oo : inout real; hoskkqti : out integer);
end wzm;

library ieee;
use ieee.std_logic_1164.all;

architecture nfszgk of wzm is
  signal ekchvd : time_vector(0 to 4);
  signal rln : time;
  signal gxriugpd : std_logic;
  signal grewbvsy : time;
  signal jmr : time_vector(0 to 4);
  signal yooaeniqc : time;
  signal zkxbfj : std_logic;
  signal rtcsgbt : time_vector(0 to 4);
  signal us : time;
  signal jajglmglle : std_logic;
begin
  bilvfepyo : entity work.jxgfzo
    port map (gchtniqjtp => jajglmglle, pjypo => us, layfuoem => rtcsgbt, nsyfsy => us);
  symrlal : entity work.jxgfzo
    port map (gchtniqjtp => zkxbfj, pjypo => yooaeniqc, layfuoem => jmr, nsyfsy => grewbvsy);
  bidtuz : entity work.jxgfzo
    port map (gchtniqjtp => gxriugpd, pjypo => rln, layfuoem => ekchvd, nsyfsy => yooaeniqc);
  
  -- Multi-driven assignments
  jajglmglle <= 'L';
  gxriugpd <= 'U';
end nfszgk;

entity cnhg is
  port (ohca : buffer time);
end cnhg;

library ieee;
use ieee.std_logic_1164.all;

architecture nlvtmr of cnhg is
  signal a : time_vector(0 to 4);
  signal cbo : time;
  signal nncpbgkema : std_logic;
begin
  qm : entity work.jxgfzo
    port map (gchtniqjtp => nncpbgkema, pjypo => cbo, layfuoem => a, nsyfsy => cbo);
  
  -- Single-driven assignments
  ohca <= 41.4_3_4 ns;
  
  -- Multi-driven assignments
  nncpbgkema <= 'U';
  nncpbgkema <= nncpbgkema;
  nncpbgkema <= 'Z';
  nncpbgkema <= '0';
end nlvtmr;

library ieee;
use ieee.std_logic_1164.all;

entity nekcpfjydg is
  port (ugpyxlshgr : inout time; splroh : linkage std_logic; gysgbt : buffer time);
end nekcpfjydg;

architecture xspicnabla of nekcpfjydg is
  signal ninsi : real;
  signal mxiqgxzp : real;
  signal gvxlvt : integer;
  signal tgl : time;
begin
  wo : entity work.cnhg
    port map (ohca => gysgbt);
  ifqdgoij : entity work.cnhg
    port map (ohca => tgl);
  ucqxzjon : entity work.wzm
    port map (jkoin => gvxlvt, uyyp => mxiqgxzp, oo => ninsi, hoskkqti => gvxlvt);
  xnqlbbr : entity work.cnhg
    port map (ohca => ugpyxlshgr);
end xspicnabla;



-- Seed after: 2291600499989025739,13501862637168280927

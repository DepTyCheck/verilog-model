-- Seed: 12405512724237189323,12260394286515585877

library ieee;
use ieee.std_logic_1164.all;

entity srcbxh is
  port (dxzle : buffer std_logic_vector(2 downto 3); evxgmv : linkage std_logic);
end srcbxh;

architecture xxwubw of srcbxh is
  
begin
  -- Multi-driven assignments
  dxzle <= dxzle;
  dxzle <= (others => '0');
  dxzle <= dxzle;
  dxzle <= dxzle;
end xxwubw;

entity blwggaulrl is
  port (wutsltud : linkage boolean_vector(3 to 0); h : in severity_level; jck : linkage real);
end blwggaulrl;

library ieee;
use ieee.std_logic_1164.all;

architecture svt of blwggaulrl is
  signal srsko : std_logic_vector(2 downto 3);
  signal cctdgxqadw : std_logic_vector(2 downto 3);
  signal vgrce : std_logic_vector(2 downto 3);
  signal a : std_logic;
  signal vxbirbd : std_logic_vector(2 downto 3);
begin
  ewl : entity work.srcbxh
    port map (dxzle => vxbirbd, evxgmv => a);
  thuglh : entity work.srcbxh
    port map (dxzle => vgrce, evxgmv => a);
  at : entity work.srcbxh
    port map (dxzle => cctdgxqadw, evxgmv => a);
  ha : entity work.srcbxh
    port map (dxzle => srsko, evxgmv => a);
  
  -- Multi-driven assignments
  srsko <= vxbirbd;
  vxbirbd <= vxbirbd;
  srsko <= "";
end svt;

library ieee;
use ieee.std_logic_1164.all;

entity smq is
  port (evkkbmt : buffer std_logic_vector(0 downto 1); s : linkage time; oecp : inout boolean; w : buffer real);
end smq;

library ieee;
use ieee.std_logic_1164.all;

architecture lnis of smq is
  signal ykmfcbiga : severity_level;
  signal b : boolean_vector(3 to 0);
  signal fhmispn : real;
  signal uvjazoo : severity_level;
  signal nvv : boolean_vector(3 to 0);
  signal bq : std_logic;
  signal vxrclejdz : std_logic_vector(2 downto 3);
  signal kwwttgl : std_logic;
begin
  mwqzajzd : entity work.srcbxh
    port map (dxzle => evkkbmt, evxgmv => kwwttgl);
  egxmmyk : entity work.srcbxh
    port map (dxzle => vxrclejdz, evxgmv => bq);
  scra : entity work.blwggaulrl
    port map (wutsltud => nvv, h => uvjazoo, jck => fhmispn);
  stnuhawwv : entity work.blwggaulrl
    port map (wutsltud => b, h => ykmfcbiga, jck => w);
  
  -- Single-driven assignments
  oecp <= FALSE;
  ykmfcbiga <= uvjazoo;
  uvjazoo <= ERROR;
  
  -- Multi-driven assignments
  bq <= kwwttgl;
  kwwttgl <= kwwttgl;
  evkkbmt <= (others => '0');
end lnis;

library ieee;
use ieee.std_logic_1164.all;

entity qplmykw is
  port (f : out integer; vw : inout integer; snhy : inout time; fmqblehlzz : out std_logic);
end qplmykw;

library ieee;
use ieee.std_logic_1164.all;

architecture kscrfvayw of qplmykw is
  signal ddeqdhcd : real;
  signal linxtupbrv : boolean;
  signal hmrnpp : time;
  signal vkcohb : std_logic;
  signal bq : std_logic_vector(0 downto 1);
begin
  jtqe : entity work.srcbxh
    port map (dxzle => bq, evxgmv => vkcohb);
  eaqdpu : entity work.smq
    port map (evkkbmt => bq, s => hmrnpp, oecp => linxtupbrv, w => ddeqdhcd);
end kscrfvayw;



-- Seed after: 13700676475443512389,12260394286515585877

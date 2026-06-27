-- Seed: 17015460759360448285,4860866131898729603

entity rfhxsqg is
  port (bxuvs : buffer time; hjgndggke : inout boolean; ilqr : inout string(4 downto 1));
end rfhxsqg;

architecture waojswarc of rfhxsqg is
  
begin
  -- Single-driven assignments
  hjgndggke <= TRUE;
  bxuvs <= 14010.0_4_0_2 ns;
  ilqr <= "piza";
end waojswarc;

entity e is
  port (l : out real);
end e;

architecture awfjyvuf of e is
  
begin
  -- Single-driven assignments
  l <= 0_1_3_4_2.41;
end awfjyvuf;

library ieee;
use ieee.std_logic_1164.all;

entity ng is
  port (zdohxpezel : out real_vector(4 downto 0); uy : linkage std_logic; ix : buffer boolean; rylmyg : out std_logic);
end ng;

architecture xhmpymignw of ng is
  signal rrris : string(4 downto 1);
  signal av : time;
  signal yonnwfurl : string(4 downto 1);
  signal prncxv : boolean;
  signal njr : time;
begin
  ojrmubp : entity work.rfhxsqg
    port map (bxuvs => njr, hjgndggke => prncxv, ilqr => yonnwfurl);
  w : entity work.rfhxsqg
    port map (bxuvs => av, hjgndggke => ix, ilqr => rrris);
  
  -- Single-driven assignments
  zdohxpezel <= (2#01011.0_0_1_0#, 2#1.1_1#, 4_0_0.103, 1_0_4.4_0_3_0_4, 4_2_4_4_2.1_0_1_3_4);
  
  -- Multi-driven assignments
  rylmyg <= '0';
end xhmpymignw;

library ieee;
use ieee.std_logic_1164.all;

entity kqfmqyomo is
  port (pvqa : buffer boolean; sm : out std_logic; s : in real; oiglt : inout character);
end kqfmqyomo;

architecture thhuftihg of kqfmqyomo is
  signal aobl : real;
  signal gvigcw : string(4 downto 1);
  signal yhapyp : time;
  signal qiuhw : string(4 downto 1);
  signal mktwhsbm : boolean;
  signal tncntfef : time;
  signal ufptxryh : string(4 downto 1);
  signal ohrqpyqd : boolean;
  signal kujdio : time;
begin
  bgy : entity work.rfhxsqg
    port map (bxuvs => kujdio, hjgndggke => ohrqpyqd, ilqr => ufptxryh);
  gsljei : entity work.rfhxsqg
    port map (bxuvs => tncntfef, hjgndggke => mktwhsbm, ilqr => qiuhw);
  qclxfxfdr : entity work.rfhxsqg
    port map (bxuvs => yhapyp, hjgndggke => pvqa, ilqr => gvigcw);
  vxmyzpd : entity work.e
    port map (l => aobl);
  
  -- Single-driven assignments
  oiglt <= 'k';
  
  -- Multi-driven assignments
  sm <= '1';
end thhuftihg;



-- Seed after: 16277978384577709422,4860866131898729603

-- Seed: 16352737727466391981,13694093582652240945

library ieee;
use ieee.std_logic_1164.all;

entity jqznndrz is
  port ( lwbveyulxn : out std_logic_vector(4 downto 2)
  ; xlbuaxct : in std_logic_vector(3 downto 3)
  ; qisscmcd : inout std_logic_vector(2 downto 0)
  ; tlxhc : out time
  );
end jqznndrz;

architecture iochdlf of jqznndrz is
  
begin
  -- Single-driven assignments
  tlxhc <= 2#0# us;
end iochdlf;

entity sqtcbm is
  port (b : buffer bit);
end sqtcbm;

library ieee;
use ieee.std_logic_1164.all;

architecture taaqhhk of sqtcbm is
  signal ijerhpnhg : time;
  signal gejbxt : std_logic_vector(3 downto 3);
  signal vmkr : time;
  signal xtsjc : std_logic_vector(4 downto 2);
  signal aorjulu : std_logic_vector(3 downto 3);
  signal hmbkkse : std_logic_vector(2 downto 0);
begin
  f : entity work.jqznndrz
    port map (lwbveyulxn => hmbkkse, xlbuaxct => aorjulu, qisscmcd => xtsjc, tlxhc => vmkr);
  ccwt : entity work.jqznndrz
    port map (lwbveyulxn => xtsjc, xlbuaxct => gejbxt, qisscmcd => hmbkkse, tlxhc => ijerhpnhg);
end taaqhhk;

library ieee;
use ieee.std_logic_1164.all;

entity o is
  port (nbkopprj : out real_vector(1 downto 0); n : in real; jljptjux : out std_logic; wbz : buffer time);
end o;

library ieee;
use ieee.std_logic_1164.all;

architecture vv of o is
  signal kcffymu : time;
  signal newnwbcus : std_logic_vector(2 downto 0);
  signal d : std_logic_vector(3 downto 3);
  signal sncwenzpt : time;
  signal uedhgwex : std_logic_vector(3 downto 3);
  signal dlehjy : std_logic_vector(3 downto 3);
  signal yqguxgci : std_logic_vector(4 downto 2);
begin
  unm : entity work.jqznndrz
    port map (lwbveyulxn => yqguxgci, xlbuaxct => dlehjy, qisscmcd => yqguxgci, tlxhc => wbz);
  ut : entity work.jqznndrz
    port map (lwbveyulxn => yqguxgci, xlbuaxct => uedhgwex, qisscmcd => yqguxgci, tlxhc => sncwenzpt);
  mcjfhoy : entity work.jqznndrz
    port map (lwbveyulxn => yqguxgci, xlbuaxct => d, qisscmcd => newnwbcus, tlxhc => kcffymu);
  
  -- Single-driven assignments
  nbkopprj <= (42.42200, 2#0_0_1_0.01#);
end vv;

entity yagrzkx is
  port (cpqsk : in real; wwr : out time; gdlchrpw : inout severity_level);
end yagrzkx;

library ieee;
use ieee.std_logic_1164.all;

architecture qytpgyzzdf of yagrzkx is
  signal bm : bit;
  signal u : time;
  signal ucvscx : std_logic_vector(2 downto 0);
  signal ngaw : std_logic_vector(3 downto 3);
  signal ir : std_logic_vector(4 downto 2);
begin
  ja : entity work.jqznndrz
    port map (lwbveyulxn => ir, xlbuaxct => ngaw, qisscmcd => ucvscx, tlxhc => u);
  ia : entity work.sqtcbm
    port map (b => bm);
  
  -- Multi-driven assignments
  ir <= "-1Z";
  ir <= ('W', 'Z', 'W');
  ir <= ('1', 'Z', '1');
end qytpgyzzdf;



-- Seed after: 3583964222008621815,13694093582652240945

-- Seed: 17038126126923448453,13694093582652240945

library ieee;
use ieee.std_logic_1164.all;

entity vpzcmlg is
  port (hvnmjnlt : buffer std_logic; nmc : inout integer; cr : buffer std_logic_vector(1 to 4));
end vpzcmlg;

architecture fva of vpzcmlg is
  
begin
  -- Single-driven assignments
  nmc <= 4_4_0;
  
  -- Multi-driven assignments
  hvnmjnlt <= 'U';
end fva;

entity wn is
  port (wvcipny : out real; agghdeiszy : in time; ro : buffer integer);
end wn;

library ieee;
use ieee.std_logic_1164.all;

architecture cxr of wn is
  signal fv : std_logic_vector(1 to 4);
  signal bnkgduvb : std_logic;
  signal botz : std_logic_vector(1 to 4);
  signal f : integer;
  signal memojpwbw : std_logic;
  signal wzsybwx : std_logic_vector(1 to 4);
  signal kwrv : integer;
  signal ii : std_logic_vector(1 to 4);
  signal irmck : integer;
  signal btumhl : std_logic;
begin
  e : entity work.vpzcmlg
    port map (hvnmjnlt => btumhl, nmc => irmck, cr => ii);
  opzcnq : entity work.vpzcmlg
    port map (hvnmjnlt => btumhl, nmc => kwrv, cr => wzsybwx);
  ttxzebhbp : entity work.vpzcmlg
    port map (hvnmjnlt => memojpwbw, nmc => f, cr => botz);
  xxrigf : entity work.vpzcmlg
    port map (hvnmjnlt => bnkgduvb, nmc => ro, cr => fv);
  
  -- Single-driven assignments
  wvcipny <= 8#4.3_2_3_4#;
  
  -- Multi-driven assignments
  btumhl <= '-';
  botz <= "X0WL";
  bnkgduvb <= 'Z';
end cxr;

entity fhwd is
  port (hcimvz : buffer real; gcaru : out severity_level; uh : buffer boolean_vector(2 downto 0); xckxfmui : linkage time);
end fhwd;

library ieee;
use ieee.std_logic_1164.all;

architecture ofdjsuk of fhwd is
  signal geadfvh : std_logic_vector(1 to 4);
  signal eypruhhnr : integer;
  signal vxnkivt : integer;
  signal knqwy : std_logic_vector(1 to 4);
  signal etnqgovp : integer;
  signal auhi : std_logic;
begin
  iurq : entity work.vpzcmlg
    port map (hvnmjnlt => auhi, nmc => etnqgovp, cr => knqwy);
  wifvm : entity work.vpzcmlg
    port map (hvnmjnlt => auhi, nmc => vxnkivt, cr => knqwy);
  oawo : entity work.vpzcmlg
    port map (hvnmjnlt => auhi, nmc => eypruhhnr, cr => geadfvh);
  
  -- Single-driven assignments
  gcaru <= ERROR;
  uh <= (TRUE, TRUE, TRUE);
  hcimvz <= 16#D1.4_B_8_3#;
end ofdjsuk;

entity rgt is
  port (vvryqfxcj : linkage bit_vector(4 downto 0); oaxnxbmpm : out string(2 downto 2); lte : out character);
end rgt;

architecture tsjlyde of rgt is
  signal dbjbpsej : integer;
  signal tpwdjipoxk : time;
  signal icvxpxt : real;
begin
  qkyfx : entity work.wn
    port map (wvcipny => icvxpxt, agghdeiszy => tpwdjipoxk, ro => dbjbpsej);
  
  -- Single-driven assignments
  oaxnxbmpm <= (others => 'b');
  lte <= 'e';
end tsjlyde;



-- Seed after: 8470337879615519795,13694093582652240945

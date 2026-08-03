-- Seed: 931719234436679348,12359743974512393525

entity qjsdwaetg is
  port (r : buffer time; psuvpgevg : in severity_level; rbnjilo : in real);
end qjsdwaetg;

architecture cwugrbyw of qjsdwaetg is
  
begin
  -- Single-driven assignments
  r <= r;
end cwugrbyw;

library ieee;
use ieee.std_logic_1164.all;

entity nuzh is
  port (hj : buffer boolean_vector(2 to 1); netxpsslaw : in time; hqc : linkage std_logic; gkjwa : inout time);
end nuzh;

architecture ettdjrho of nuzh is
  signal fhdrpmaanp : real;
  signal a : real;
  signal hcgkzftr : severity_level;
  signal arjemet : time;
  signal hxpoqv : real;
  signal aaotyolj : severity_level;
  signal rx : time;
begin
  foipu : entity work.qjsdwaetg
    port map (r => rx, psuvpgevg => aaotyolj, rbnjilo => hxpoqv);
  xoyk : entity work.qjsdwaetg
    port map (r => arjemet, psuvpgevg => hcgkzftr, rbnjilo => a);
  cf : entity work.qjsdwaetg
    port map (r => gkjwa, psuvpgevg => hcgkzftr, rbnjilo => fhdrpmaanp);
end ettdjrho;

library ieee;
use ieee.std_logic_1164.all;

entity zv is
  port (oefzowed : linkage real; as : out std_logic_vector(3 downto 0); cqluluopzq : out real; pth : buffer time);
end zv;

library ieee;
use ieee.std_logic_1164.all;

architecture tqnrfxcuu of zv is
  signal ozzdvswuoe : severity_level;
  signal tcbuq : time;
  signal iyqs : severity_level;
  signal fg : time;
  signal ebjfvuy : std_logic;
  signal vveoxhwdfg : time;
  signal jxk : boolean_vector(2 to 1);
  signal pjl : std_logic;
  signal wozavakxv : time;
  signal qyuywjah : boolean_vector(2 to 1);
begin
  lpcvxfin : entity work.nuzh
    port map (hj => qyuywjah, netxpsslaw => wozavakxv, hqc => pjl, gkjwa => wozavakxv);
  i : entity work.nuzh
    port map (hj => jxk, netxpsslaw => vveoxhwdfg, hqc => ebjfvuy, gkjwa => fg);
  ecughw : entity work.qjsdwaetg
    port map (r => pth, psuvpgevg => iyqs, rbnjilo => cqluluopzq);
  jfomtjh : entity work.qjsdwaetg
    port map (r => tcbuq, psuvpgevg => ozzdvswuoe, rbnjilo => cqluluopzq);
  
  -- Multi-driven assignments
  as <= ('X', 'U', '0', 'H');
  as <= "H1W-";
end tqnrfxcuu;



-- Seed after: 6153281234403776026,12359743974512393525

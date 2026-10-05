-- Seed: 2036329281970684862,7304262412290825129

library ieee;
use ieee.std_logic_1164.all;

entity e is
  port (bf : in integer; ojibu : linkage std_logic_vector(1 downto 4); ztxbtrqtcs : inout time; xqsoxk : buffer std_logic);
end e;

architecture gioy of e is
  
begin
  -- Single-driven assignments
  ztxbtrqtcs <= ztxbtrqtcs;
  
  -- Multi-driven assignments
  xqsoxk <= xqsoxk;
end gioy;

library ieee;
use ieee.std_logic_1164.all;

entity bwu is
  port (gprbcaiai : buffer integer; mtbccd : in std_logic; szq : out time);
end bwu;

library ieee;
use ieee.std_logic_1164.all;

architecture lkyamqjlxx of bwu is
  signal botrz : time;
  signal lmil : std_logic;
  signal gp : std_logic_vector(1 downto 4);
  signal ub : std_logic;
  signal slgwnat : time;
  signal r : std_logic_vector(1 downto 4);
  signal igmjcwg : integer;
begin
  d : entity work.e
    port map (bf => igmjcwg, ojibu => r, ztxbtrqtcs => slgwnat, xqsoxk => ub);
  xpvu : entity work.e
    port map (bf => gprbcaiai, ojibu => gp, ztxbtrqtcs => szq, xqsoxk => lmil);
  npu : entity work.e
    port map (bf => igmjcwg, ojibu => gp, ztxbtrqtcs => botrz, xqsoxk => ub);
  
  -- Single-driven assignments
  gprbcaiai <= gprbcaiai;
  
  -- Multi-driven assignments
  lmil <= 'W';
  gp <= (others => '0');
  gp <= (others => '0');
  gp <= (others => '0');
end lkyamqjlxx;

entity nkf is
  port (yshdp : linkage boolean_vector(3 downto 1); gipukw : in time; z : in severity_level);
end nkf;

library ieee;
use ieee.std_logic_1164.all;

architecture uiz of nkf is
  signal qj : time;
  signal czbeugjznd : std_logic;
  signal yuov : time;
  signal tpls : std_logic_vector(1 downto 4);
  signal vvf : integer;
begin
  wt : entity work.e
    port map (bf => vvf, ojibu => tpls, ztxbtrqtcs => yuov, xqsoxk => czbeugjznd);
  expwsiz : entity work.bwu
    port map (gprbcaiai => vvf, mtbccd => czbeugjznd, szq => qj);
  
  -- Multi-driven assignments
  tpls <= "";
end uiz;

entity epvytw is
  port (qi : out severity_level; tmjbnfty : out time; pdefumbhef : linkage real; agoz : inout time);
end epvytw;

library ieee;
use ieee.std_logic_1164.all;

architecture q of epvytw is
  signal uf : std_logic;
  signal eddorg : integer;
  signal sfmmzndo : time;
  signal mnegosg : boolean_vector(3 downto 1);
begin
  g : entity work.nkf
    port map (yshdp => mnegosg, gipukw => sfmmzndo, z => qi);
  chevtgl : entity work.bwu
    port map (gprbcaiai => eddorg, mtbccd => uf, szq => tmjbnfty);
  
  -- Single-driven assignments
  qi <= NOTE;
  agoz <= 16#0_B# ps;
  sfmmzndo <= agoz;
  
  -- Multi-driven assignments
  uf <= uf;
  uf <= uf;
  uf <= '-';
  uf <= 'L';
end q;



-- Seed after: 10208046960040428006,7304262412290825129

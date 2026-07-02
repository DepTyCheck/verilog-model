-- Seed: 3549072050784906867,13694093582652240945

library ieee;
use ieee.std_logic_1164.all;

entity a is
  port (k : inout std_logic; hkoqqdlxc : out time; bvfykbqfn : in integer; y : out integer);
end a;

architecture u of a is
  
begin
  -- Single-driven assignments
  y <= 2;
end u;

entity n is
  port (efrhsdn : out time_vector(2 downto 2));
end n;

library ieee;
use ieee.std_logic_1164.all;

architecture qnfqwcjceg of n is
  signal z : time;
  signal wuuwwd : std_logic;
  signal ql : integer;
  signal q : time;
  signal uciehyx : integer;
  signal wj : integer;
  signal ld : time;
  signal kbgvgwo : std_logic;
begin
  mckwb : entity work.a
    port map (k => kbgvgwo, hkoqqdlxc => ld, bvfykbqfn => wj, y => uciehyx);
  hlcvmkaj : entity work.a
    port map (k => kbgvgwo, hkoqqdlxc => q, bvfykbqfn => wj, y => ql);
  oklw : entity work.a
    port map (k => wuuwwd, hkoqqdlxc => z, bvfykbqfn => uciehyx, y => wj);
  
  -- Multi-driven assignments
  kbgvgwo <= '1';
  wuuwwd <= '1';
  wuuwwd <= 'Z';
end qnfqwcjceg;

library ieee;
use ieee.std_logic_1164.all;

entity vwofpyehv is
  port (swfzzv : out std_logic_vector(0 to 3); ehmpjfky : out real);
end vwofpyehv;

library ieee;
use ieee.std_logic_1164.all;

architecture omabgg of vwofpyehv is
  signal ixfwlzza : integer;
  signal kef : time;
  signal rmisbvrdc : std_logic;
  signal tznysl : time_vector(2 downto 2);
begin
  yztkydjcdu : entity work.n
    port map (efrhsdn => tznysl);
  xxmjuoty : entity work.a
    port map (k => rmisbvrdc, hkoqqdlxc => kef, bvfykbqfn => ixfwlzza, y => ixfwlzza);
  
  -- Single-driven assignments
  ehmpjfky <= 2_1_3.44224;
  
  -- Multi-driven assignments
  rmisbvrdc <= '0';
end omabgg;

entity xulyiipf is
  port (yhbblstbg : in time; ii : out boolean);
end xulyiipf;

library ieee;
use ieee.std_logic_1164.all;

architecture vnfnsxch of xulyiipf is
  signal hpjakka : integer;
  signal jt : time;
  signal jmf : integer;
  signal vud : integer;
  signal ymuwacarv : time;
  signal ytiay : std_logic;
begin
  pnitliev : entity work.a
    port map (k => ytiay, hkoqqdlxc => ymuwacarv, bvfykbqfn => vud, y => jmf);
  lvouwj : entity work.a
    port map (k => ytiay, hkoqqdlxc => jt, bvfykbqfn => hpjakka, y => hpjakka);
  
  -- Single-driven assignments
  ii <= TRUE;
  vud <= 4;
  
  -- Multi-driven assignments
  ytiay <= 'L';
  ytiay <= '1';
end vnfnsxch;



-- Seed after: 5598356322061153042,13694093582652240945

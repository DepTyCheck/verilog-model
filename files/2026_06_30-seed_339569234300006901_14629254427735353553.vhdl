-- Seed: 339569234300006901,14629254427735353553

entity ckssmjjt is
  port (ptfob : linkage time);
end ckssmjjt;

architecture qavo of ckssmjjt is
  
begin
  
end qavo;

entity lv is
  port (rsuxqypp : out bit_vector(2 downto 0); klm : linkage real; cm : in bit);
end lv;

architecture ph of lv is
  signal qlgzg : time;
  signal pndzymyup : time;
  signal mim : time;
begin
  q : entity work.ckssmjjt
    port map (ptfob => mim);
  cmjwgqmqaz : entity work.ckssmjjt
    port map (ptfob => pndzymyup);
  j : entity work.ckssmjjt
    port map (ptfob => qlgzg);
  
  -- Single-driven assignments
  rsuxqypp <= ('1', '0', '1');
end ph;

library ieee;
use ieee.std_logic_1164.all;

entity oeekldrwn is
  port (pvgwysvd : in time_vector(3 downto 2); ipvlkgz : in std_logic_vector(4 downto 3); gqrocvi : linkage time);
end oeekldrwn;

architecture lvoh of oeekldrwn is
  signal lzheb : real;
  signal d : bit_vector(2 downto 0);
  signal ce : bit;
  signal farbsol : real;
  signal vvylpwk : bit_vector(2 downto 0);
begin
  tyima : entity work.lv
    port map (rsuxqypp => vvylpwk, klm => farbsol, cm => ce);
  mholllequ : entity work.lv
    port map (rsuxqypp => d, klm => lzheb, cm => ce);
  s : entity work.ckssmjjt
    port map (ptfob => gqrocvi);
  
  -- Single-driven assignments
  ce <= '1';
end lvoh;



-- Seed after: 358761903654597472,14629254427735353553

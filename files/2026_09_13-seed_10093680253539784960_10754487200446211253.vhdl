-- Seed: 10093680253539784960,10754487200446211253

library ieee;
use ieee.std_logic_1164.all;

entity zs is
  port (c : in integer_vector(3 downto 2); squdhrbm : inout std_logic);
end zs;

architecture qajyvwq of zs is
  
begin
  -- Multi-driven assignments
  squdhrbm <= squdhrbm;
  squdhrbm <= '1';
  squdhrbm <= '0';
end qajyvwq;

entity kakbzfs is
  port (stfmfkrj : out integer);
end kakbzfs;

library ieee;
use ieee.std_logic_1164.all;

architecture ourac of kakbzfs is
  signal i : std_logic;
  signal ww : integer_vector(3 downto 2);
begin
  bpwggd : entity work.zs
    port map (c => ww, squdhrbm => i);
  ccxvqg : entity work.zs
    port map (c => ww, squdhrbm => i);
  
  -- Single-driven assignments
  ww <= (32, 8#4167#);
  stfmfkrj <= stfmfkrj;
  
  -- Multi-driven assignments
  i <= 'U';
  i <= i;
end ourac;

library ieee;
use ieee.std_logic_1164.all;

entity euvrndc is
  port (efax : in std_logic);
end euvrndc;

library ieee;
use ieee.std_logic_1164.all;

architecture l of euvrndc is
  signal mouc : std_logic;
  signal odxwymkxc : integer_vector(3 downto 2);
begin
  khhilinbkl : entity work.zs
    port map (c => odxwymkxc, squdhrbm => mouc);
  
  -- Single-driven assignments
  odxwymkxc <= odxwymkxc;
  
  -- Multi-driven assignments
  mouc <= efax;
  mouc <= mouc;
  mouc <= 'H';
end l;



-- Seed after: 10599561537245055296,10754487200446211253

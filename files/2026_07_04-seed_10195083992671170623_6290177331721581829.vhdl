-- Seed: 10195083992671170623,6290177331721581829

library ieee;
use ieee.std_logic_1164.all;

entity kclrrc is
  port (ubvwngdxul : buffer real; kcegytrpq : inout std_logic_vector(2 downto 3); enp : out std_logic_vector(3 downto 2));
end kclrrc;

architecture vebfco of kclrrc is
  
begin
  -- Single-driven assignments
  ubvwngdxul <= ubvwngdxul;
  
  -- Multi-driven assignments
  kcegytrpq <= (others => '0');
  enp <= "X1";
end vebfco;

entity alr is
  port (zi : out time);
end alr;

library ieee;
use ieee.std_logic_1164.all;

architecture jmucjqi of alr is
  signal twqmgi : std_logic_vector(3 downto 2);
  signal sw : real;
  signal lmf : std_logic_vector(3 downto 2);
  signal eorvc : std_logic_vector(2 downto 3);
  signal wgqn : real;
begin
  xbon : entity work.kclrrc
    port map (ubvwngdxul => wgqn, kcegytrpq => eorvc, enp => lmf);
  rwbjajtti : entity work.kclrrc
    port map (ubvwngdxul => sw, kcegytrpq => eorvc, enp => twqmgi);
  
  -- Single-driven assignments
  zi <= zi;
  
  -- Multi-driven assignments
  twqmgi <= ('Z', 'Z');
  lmf <= lmf;
  eorvc <= eorvc;
end jmucjqi;



-- Seed after: 7788876615588363808,6290177331721581829

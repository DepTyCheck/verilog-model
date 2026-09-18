-- Seed: 1063251808334800424,3316342841050048249

library ieee;
use ieee.std_logic_1164.all;

entity zcybd is
  port (bvpylpg : in std_logic_vector(4 downto 4); rm : buffer std_logic; q : out real);
end zcybd;

architecture a of zcybd is
  
begin
  -- Single-driven assignments
  q <= q;
  
  -- Multi-driven assignments
  rm <= rm;
  rm <= 'Z';
  rm <= 'U';
  rm <= 'U';
end a;

entity a is
  port (ndebni : out severity_level; jstj : in time);
end a;

library ieee;
use ieee.std_logic_1164.all;

architecture qpoz of a is
  signal xawqaojrv : real;
  signal tvdyq : std_logic;
  signal y : std_logic_vector(4 downto 4);
  signal bpqeifxoj : real;
  signal qbmij : std_logic_vector(4 downto 4);
  signal shtwcuiv : real;
  signal ctqdlzopyc : std_logic;
  signal w : std_logic_vector(4 downto 4);
begin
  vghhkis : entity work.zcybd
    port map (bvpylpg => w, rm => ctqdlzopyc, q => shtwcuiv);
  eudzx : entity work.zcybd
    port map (bvpylpg => qbmij, rm => ctqdlzopyc, q => bpqeifxoj);
  nhjkwx : entity work.zcybd
    port map (bvpylpg => y, rm => tvdyq, q => xawqaojrv);
  
  -- Single-driven assignments
  ndebni <= NOTE;
end qpoz;



-- Seed after: 4887102268935942797,3316342841050048249

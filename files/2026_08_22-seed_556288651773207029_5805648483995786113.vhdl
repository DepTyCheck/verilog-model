-- Seed: 556288651773207029,5805648483995786113

entity ez is
  port (zlua : buffer time);
end ez;

architecture lrdvkurwd of ez is
  
begin
  -- Single-driven assignments
  zlua <= zlua;
end lrdvkurwd;

library ieee;
use ieee.std_logic_1164.all;

entity spbjz is
  port (s : inout std_logic; bmhcec : inout std_logic);
end spbjz;

architecture x of spbjz is
  signal mihrtt : time;
begin
  chnpx : entity work.ez
    port map (zlua => mihrtt);
  
  -- Multi-driven assignments
  s <= bmhcec;
  bmhcec <= '0';
end x;

library ieee;
use ieee.std_logic_1164.all;

entity f is
  port (krefbuvp : out time_vector(3 to 4); wpk : inout real; njoww : inout std_logic_vector(1 downto 1); ub : in integer_vector(2 downto 1));
end f;

architecture c of f is
  signal gb : time;
  signal nnrimyfuc : time;
begin
  but : entity work.ez
    port map (zlua => nnrimyfuc);
  qkbsziiy : entity work.ez
    port map (zlua => gb);
  
  -- Single-driven assignments
  wpk <= wpk;
  krefbuvp <= (0 min, 2#001# ms);
  
  -- Multi-driven assignments
  njoww <= "Z";
end c;



-- Seed after: 13967152925845218900,5805648483995786113

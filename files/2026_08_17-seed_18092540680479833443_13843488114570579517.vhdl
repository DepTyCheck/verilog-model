-- Seed: 18092540680479833443,13843488114570579517

library ieee;
use ieee.std_logic_1164.all;

entity fxkhurncqo is
  port ( zx : buffer time_vector(0 downto 2)
  ; zksafp : in std_logic_vector(2 downto 1)
  ; ncnxmtusz : inout integer
  ; zelinfagt : in boolean_vector(1 to 0)
  );
end fxkhurncqo;

architecture o of fxkhurncqo is
  
begin
  -- Single-driven assignments
  zx <= (others => 0 ns);
  ncnxmtusz <= 4132;
end o;

library ieee;
use ieee.std_logic_1164.all;

entity dcs is
  port (cggevnm : in std_logic; exjf : out std_logic);
end dcs;

library ieee;
use ieee.std_logic_1164.all;

architecture ail of dcs is
  signal qkuprl : boolean_vector(1 to 0);
  signal ndssbraibe : integer;
  signal bh : time_vector(0 downto 2);
  signal a : boolean_vector(1 to 0);
  signal klxoid : integer;
  signal oxfdm : std_logic_vector(2 downto 1);
  signal urbazu : time_vector(0 downto 2);
begin
  ty : entity work.fxkhurncqo
    port map (zx => urbazu, zksafp => oxfdm, ncnxmtusz => klxoid, zelinfagt => a);
  gpbwmvdo : entity work.fxkhurncqo
    port map (zx => bh, zksafp => oxfdm, ncnxmtusz => ndssbraibe, zelinfagt => qkuprl);
  
  -- Single-driven assignments
  qkuprl <= a;
  
  -- Multi-driven assignments
  oxfdm <= oxfdm;
  exjf <= 'U';
  oxfdm <= ('1', '0');
  exjf <= exjf;
end ail;



-- Seed after: 6938686960251345284,13843488114570579517

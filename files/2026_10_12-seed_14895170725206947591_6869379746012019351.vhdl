-- Seed: 14895170725206947591,6869379746012019351

library ieee;
use ieee.std_logic_1164.all;

entity rxsvplux is
  port (mwuaso : in std_logic_vector(0 to 3); zkjphlsu : inout boolean; asmxkdik : inout std_logic_vector(1 to 2));
end rxsvplux;

architecture fbkkiecqk of rxsvplux is
  
begin
  -- Single-driven assignments
  zkjphlsu <= FALSE;
  
  -- Multi-driven assignments
  asmxkdik <= ('1', 'L');
  asmxkdik <= ('W', 'L');
  asmxkdik <= ('L', 'H');
end fbkkiecqk;

entity hvzlxa is
  port (mkaad : buffer integer);
end hvzlxa;

library ieee;
use ieee.std_logic_1164.all;

architecture qm of hvzlxa is
  signal ltqn : std_logic_vector(1 to 2);
  signal ma : boolean;
  signal hhvd : std_logic_vector(0 to 3);
begin
  fbby : entity work.rxsvplux
    port map (mwuaso => hhvd, zkjphlsu => ma, asmxkdik => ltqn);
  
  -- Single-driven assignments
  mkaad <= 0;
  
  -- Multi-driven assignments
  hhvd <= "XZ-X";
  hhvd <= ('-', '0', '0', 'H');
  ltqn <= "LX";
  hhvd <= hhvd;
end qm;



-- Seed after: 9717818549977257681,6869379746012019351

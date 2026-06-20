-- Seed: 2177082966536352800,3924983747739634027

library ieee;
use ieee.std_logic_1164.all;

entity ewocsas is
  port (syumhm : linkage std_logic; iocyntnaan : inout integer);
end ewocsas;

architecture vr of ewocsas is
  
begin
  -- Single-driven assignments
  iocyntnaan <= 8#5_4_4#;
end vr;

entity nu is
  port (hmuly : in severity_level);
end nu;

library ieee;
use ieee.std_logic_1164.all;

architecture lwusyliqmm of nu is
  signal wvsdd : integer;
  signal kwkoy : std_logic;
  signal xzccrt : integer;
  signal gatjqesr : std_logic;
begin
  ltvrflsh : entity work.ewocsas
    port map (syumhm => gatjqesr, iocyntnaan => xzccrt);
  hvoifk : entity work.ewocsas
    port map (syumhm => kwkoy, iocyntnaan => wvsdd);
  
  -- Multi-driven assignments
  gatjqesr <= 'Z';
  kwkoy <= 'W';
  gatjqesr <= 'X';
  kwkoy <= '-';
end lwusyliqmm;



-- Seed after: 4543901537109514799,3924983747739634027

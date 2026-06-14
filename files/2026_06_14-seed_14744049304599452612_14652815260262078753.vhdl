-- Seed: 14744049304599452612,14652815260262078753

entity d is
  port (ulgr : out boolean_vector(2 downto 1); zorigd : in real);
end d;

architecture i of d is
  
begin
  -- Single-driven assignments
  ulgr <= (TRUE, FALSE);
end i;

library ieee;
use ieee.std_logic_1164.all;

entity rbvrgyywr is
  port (pad : in std_logic; kel : linkage real);
end rbvrgyywr;

architecture oqnrc of rbvrgyywr is
  signal jofa : real;
  signal hnuosbeu : boolean_vector(2 downto 1);
  signal ldrniy : boolean_vector(2 downto 1);
  signal bfrtwi : real;
  signal vpgh : boolean_vector(2 downto 1);
begin
  bovut : entity work.d
    port map (ulgr => vpgh, zorigd => bfrtwi);
  r : entity work.d
    port map (ulgr => ldrniy, zorigd => bfrtwi);
  fyb : entity work.d
    port map (ulgr => hnuosbeu, zorigd => jofa);
  
  -- Single-driven assignments
  bfrtwi <= 04220.3;
end oqnrc;



-- Seed after: 15656117869513159263,14652815260262078753

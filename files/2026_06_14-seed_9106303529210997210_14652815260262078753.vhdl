-- Seed: 9106303529210997210,14652815260262078753

library ieee;
use ieee.std_logic_1164.all;

entity jhjfaqm is
  port (wyei : in std_logic_vector(4 to 1); symyiogm : linkage real; yzcwi : inout integer);
end jhjfaqm;

architecture ffzwv of jhjfaqm is
  
begin
  -- Single-driven assignments
  yzcwi <= 16#5_3_0#;
end ffzwv;

library ieee;
use ieee.std_logic_1164.all;

entity ftqeait is
  port (ieudz : inout std_logic_vector(3 downto 3); pgdekp : buffer real);
end ftqeait;

library ieee;
use ieee.std_logic_1164.all;

architecture zehl of ftqeait is
  signal yjmujuy : integer;
  signal uylfwxg : std_logic_vector(4 to 1);
begin
  x : entity work.jhjfaqm
    port map (wyei => uylfwxg, symyiogm => pgdekp, yzcwi => yjmujuy);
  
  -- Multi-driven assignments
  ieudz <= (others => 'Z');
  ieudz <= (others => 'X');
  ieudz <= "Z";
  ieudz <= (others => '-');
end zehl;

library ieee;
use ieee.std_logic_1164.all;

entity mxq is
  port (ulqes : in std_logic_vector(4 to 0); ewgyg : buffer std_logic);
end mxq;

library ieee;
use ieee.std_logic_1164.all;

architecture olhoewhdy of mxq is
  signal wm : real;
  signal nuzeuaj : std_logic_vector(3 downto 3);
begin
  xvxwotd : entity work.ftqeait
    port map (ieudz => nuzeuaj, pgdekp => wm);
end olhoewhdy;



-- Seed after: 15683104930111152304,14652815260262078753

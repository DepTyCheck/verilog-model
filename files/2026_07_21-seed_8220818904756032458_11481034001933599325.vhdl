-- Seed: 8220818904756032458,11481034001933599325

library ieee;
use ieee.std_logic_1164.all;

entity wemq is
  port (rwd : in string(3 to 3); cnjhmndwzp : buffer std_logic);
end wemq;

architecture ekcuveuoq of wemq is
  
begin
  
end ekcuveuoq;

library ieee;
use ieee.std_logic_1164.all;

entity zjmmmhvna is
  port (rci : out time; sgu : out std_logic_vector(0 to 0); csaymzrc : out std_logic_vector(4 downto 2); nixdrlh : in std_logic_vector(0 to 1));
end zjmmmhvna;

library ieee;
use ieee.std_logic_1164.all;

architecture iullib of zjmmmhvna is
  signal jtiwsxleou : string(3 to 3);
  signal lq : std_logic;
  signal xxepgeb : std_logic;
  signal rvtjvspjb : string(3 to 3);
begin
  hejkslxwr : entity work.wemq
    port map (rwd => rvtjvspjb, cnjhmndwzp => xxepgeb);
  ishreqxe : entity work.wemq
    port map (rwd => rvtjvspjb, cnjhmndwzp => lq);
  pwajzq : entity work.wemq
    port map (rwd => jtiwsxleou, cnjhmndwzp => xxepgeb);
  
  -- Single-driven assignments
  rci <= rci;
  jtiwsxleou <= "r";
  rvtjvspjb <= (others => 'c');
  
  -- Multi-driven assignments
  xxepgeb <= 'W';
  lq <= xxepgeb;
  csaymzrc <= csaymzrc;
end iullib;



-- Seed after: 16544895546936048672,11481034001933599325

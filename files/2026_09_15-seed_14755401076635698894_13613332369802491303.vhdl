-- Seed: 14755401076635698894,13613332369802491303

entity wwoy is
  port (bzs : inout real);
end wwoy;

architecture nygcrklgz of wwoy is
  
begin
  -- Single-driven assignments
  bzs <= bzs;
end nygcrklgz;

library ieee;
use ieee.std_logic_1164.all;

entity rjrlktqp is
  port (livkxmmlv : inout std_logic);
end rjrlktqp;

architecture udh of rjrlktqp is
  
begin
  -- Multi-driven assignments
  livkxmmlv <= 'W';
  livkxmmlv <= livkxmmlv;
  livkxmmlv <= livkxmmlv;
  livkxmmlv <= livkxmmlv;
end udh;

entity yvg is
  port (ieac : buffer real_vector(1 downto 0));
end yvg;

library ieee;
use ieee.std_logic_1164.all;

architecture llmq of yvg is
  signal jncpdcu : std_logic;
  signal rflf : std_logic;
  signal wcpavo : real;
begin
  n : entity work.wwoy
    port map (bzs => wcpavo);
  mnzztvo : entity work.rjrlktqp
    port map (livkxmmlv => rflf);
  dklfx : entity work.rjrlktqp
    port map (livkxmmlv => jncpdcu);
  
  -- Single-driven assignments
  ieac <= (2#11010.00#, 8#6.5_5#);
  
  -- Multi-driven assignments
  rflf <= 'U';
  jncpdcu <= 'W';
  jncpdcu <= 'Z';
end llmq;



-- Seed after: 15435759846187819355,13613332369802491303

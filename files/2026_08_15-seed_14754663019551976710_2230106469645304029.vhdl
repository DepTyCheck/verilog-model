-- Seed: 14754663019551976710,2230106469645304029

library ieee;
use ieee.std_logic_1164.all;

entity qva is
  port (kntvwzbsg : inout std_logic);
end qva;

architecture ihlqvpg of qva is
  
begin
  -- Multi-driven assignments
  kntvwzbsg <= 'Z';
  kntvwzbsg <= kntvwzbsg;
end ihlqvpg;

library ieee;
use ieee.std_logic_1164.all;

entity zkwlcbjmjx is
  port (n : buffer std_logic_vector(0 to 4));
end zkwlcbjmjx;

library ieee;
use ieee.std_logic_1164.all;

architecture sqsqmznz of zkwlcbjmjx is
  signal dlirw : std_logic;
  signal kxeocti : std_logic;
begin
  cgo : entity work.qva
    port map (kntvwzbsg => kxeocti);
  jmpfwyrlm : entity work.qva
    port map (kntvwzbsg => kxeocti);
  hthyyhd : entity work.qva
    port map (kntvwzbsg => dlirw);
  tvrhxi : entity work.qva
    port map (kntvwzbsg => kxeocti);
  
  -- Multi-driven assignments
  kxeocti <= dlirw;
  dlirw <= kxeocti;
  n <= ('L', 'X', 'X', 'H', 'W');
end sqsqmznz;

library ieee;
use ieee.std_logic_1164.all;

entity f is
  port (xxnieslcc : in std_logic; pdfegej : inout string(1 downto 3));
end f;

library ieee;
use ieee.std_logic_1164.all;

architecture x of f is
  signal skyl : std_logic;
  signal aeyxlcti : std_logic_vector(0 to 4);
begin
  wc : entity work.zkwlcbjmjx
    port map (n => aeyxlcti);
  kahmkwbf : entity work.qva
    port map (kntvwzbsg => skyl);
  
  -- Single-driven assignments
  pdfegej <= (others => ' ');
  
  -- Multi-driven assignments
  aeyxlcti <= "-WXW1";
  skyl <= xxnieslcc;
  aeyxlcti <= "1Z-LX";
  aeyxlcti <= aeyxlcti;
end x;



-- Seed after: 16261229947843198886,2230106469645304029

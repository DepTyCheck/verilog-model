-- Seed: 12779283227046997124,14652815260262078753

library ieee;
use ieee.std_logic_1164.all;

entity rufomb is
  port (amlayehx : out std_logic_vector(1 to 0); bhrbdzh : in boolean; ngtv : buffer integer);
end rufomb;

architecture yhki of rufomb is
  
begin
  -- Single-driven assignments
  ngtv <= 2#1#;
  
  -- Multi-driven assignments
  amlayehx <= (others => '0');
  amlayehx <= (others => '0');
  amlayehx <= (others => '0');
  amlayehx <= "";
end yhki;

library ieee;
use ieee.std_logic_1164.all;

entity tmxci is
  port (kmrkridhg : out std_logic; jmsffxyzim : in std_logic_vector(4 to 2));
end tmxci;

library ieee;
use ieee.std_logic_1164.all;

architecture cxcdnld of tmxci is
  signal xbvjwqc : integer;
  signal ee : boolean;
  signal xhxq : std_logic_vector(1 to 0);
begin
  aqxzzth : entity work.rufomb
    port map (amlayehx => xhxq, bhrbdzh => ee, ngtv => xbvjwqc);
  
  -- Single-driven assignments
  ee <= FALSE;
  
  -- Multi-driven assignments
  kmrkridhg <= 'W';
  xhxq <= "";
  kmrkridhg <= 'L';
end cxcdnld;

library ieee;
use ieee.std_logic_1164.all;

entity wahzi is
  port (rawtwr : buffer std_logic; hjx : out boolean; x : inout std_logic_vector(2 downto 3));
end wahzi;

library ieee;
use ieee.std_logic_1164.all;

architecture bgnzyv of wahzi is
  signal pgzh : integer;
  signal pdfql : boolean;
  signal chinjxgzn : std_logic_vector(1 to 0);
  signal ztszvjktpu : integer;
  signal a : std_logic_vector(1 to 0);
begin
  v : entity work.rufomb
    port map (amlayehx => a, bhrbdzh => hjx, ngtv => ztszvjktpu);
  z : entity work.rufomb
    port map (amlayehx => chinjxgzn, bhrbdzh => pdfql, ngtv => pgzh);
  
  -- Single-driven assignments
  pdfql <= FALSE;
  hjx <= TRUE;
  
  -- Multi-driven assignments
  x <= (others => '0');
end bgnzyv;



-- Seed after: 13894468172835157277,14652815260262078753

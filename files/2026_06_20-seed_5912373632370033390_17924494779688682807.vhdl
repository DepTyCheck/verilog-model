-- Seed: 5912373632370033390,17924494779688682807

library ieee;
use ieee.std_logic_1164.all;

entity a is
  port (mivsgbshm : out integer; iofbvzct : buffer std_logic);
end a;

architecture i of a is
  
begin
  -- Single-driven assignments
  mivsgbshm <= 8#5_4_5#;
  
  -- Multi-driven assignments
  iofbvzct <= '1';
  iofbvzct <= 'W';
  iofbvzct <= 'L';
  iofbvzct <= '1';
end i;

entity cyla is
  port (bxswsvb : inout integer);
end cyla;

library ieee;
use ieee.std_logic_1164.all;

architecture lekyjf of cyla is
  signal bfa : std_logic;
  signal hnqu : std_logic;
  signal ivfdjnlzyq : integer;
begin
  wejhxdyfjv : entity work.a
    port map (mivsgbshm => ivfdjnlzyq, iofbvzct => hnqu);
  lgpdwvpqlx : entity work.a
    port map (mivsgbshm => bxswsvb, iofbvzct => bfa);
  
  -- Multi-driven assignments
  hnqu <= 'H';
  hnqu <= 'L';
  hnqu <= 'L';
end lekyjf;



-- Seed after: 5493935895450938552,17924494779688682807

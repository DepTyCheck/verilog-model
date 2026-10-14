-- Seed: 17338290577246395091,2287986438313874379

entity gjqpkwpib is
  port (t : out integer);
end gjqpkwpib;

architecture h of gjqpkwpib is
  
begin
  -- Single-driven assignments
  t <= t;
end h;

library ieee;
use ieee.std_logic_1164.all;

entity ipsdzlsjhj is
  port (wsngzasuqa : buffer std_logic; fwwxnx : linkage std_logic);
end ipsdzlsjhj;

architecture ry of ipsdzlsjhj is
  signal c : integer;
begin
  byz : entity work.gjqpkwpib
    port map (t => c);
end ry;

library ieee;
use ieee.std_logic_1164.all;

entity dhlql is
  port (k : in character; wdivtznd : in character; lpbhyzne : buffer std_logic_vector(0 downto 3); elbqvvo : in std_logic_vector(3 to 0));
end dhlql;

library ieee;
use ieee.std_logic_1164.all;

architecture zflsz of dhlql is
  signal yauntqgfds : std_logic;
  signal i : std_logic;
begin
  u : entity work.ipsdzlsjhj
    port map (wsngzasuqa => i, fwwxnx => yauntqgfds);
  
  -- Multi-driven assignments
  lpbhyzne <= elbqvvo;
  yauntqgfds <= i;
end zflsz;

library ieee;
use ieee.std_logic_1164.all;

entity iyly is
  port (ljdehmh : inout std_logic_vector(4 to 4); b : buffer std_logic; krohwuaq : out std_logic);
end iyly;

architecture gkfwrkyasn of iyly is
  signal qtbnrobyq : integer;
begin
  ved : entity work.ipsdzlsjhj
    port map (wsngzasuqa => krohwuaq, fwwxnx => krohwuaq);
  zjbs : entity work.gjqpkwpib
    port map (t => qtbnrobyq);
end gkfwrkyasn;



-- Seed after: 15432169161945364315,2287986438313874379

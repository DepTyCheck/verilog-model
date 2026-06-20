-- Seed: 7700264775203594586,17924494779688682807

library ieee;
use ieee.std_logic_1164.all;

entity mrw is
  port (stlyefmzqb : inout std_logic_vector(2 downto 1); eonmdam : out integer; fk : in time; ktmvkmv : inout real);
end mrw;

architecture pp of mrw is
  
begin
  -- Single-driven assignments
  ktmvkmv <= 34212.30432;
  eonmdam <= 16#B#;
end pp;

library ieee;
use ieee.std_logic_1164.all;

entity c is
  port (zwreyquhw : buffer boolean; h : linkage std_logic);
end c;

library ieee;
use ieee.std_logic_1164.all;

architecture lwzv of c is
  signal kzphvzac : real;
  signal gkhcrdfpds : time;
  signal aob : integer;
  signal hcdxksp : std_logic_vector(2 downto 1);
begin
  eiz : entity work.mrw
    port map (stlyefmzqb => hcdxksp, eonmdam => aob, fk => gkhcrdfpds, ktmvkmv => kzphvzac);
  
  -- Single-driven assignments
  zwreyquhw <= TRUE;
  gkhcrdfpds <= 2#100.1_1_0# us;
  
  -- Multi-driven assignments
  hcdxksp <= "0-";
end lwzv;



-- Seed after: 16289570257110202354,17924494779688682807

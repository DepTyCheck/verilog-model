-- Seed: 12399538848507779856,17924494779688682807

library ieee;
use ieee.std_logic_1164.all;

entity toqpvu is
  port (jwcauta : in bit; wcumijw : in severity_level; a : in real_vector(2 to 0); gqnprg : inout std_logic);
end toqpvu;

architecture gmkqwgnpqb of toqpvu is
  
begin
  -- Multi-driven assignments
  gqnprg <= '0';
  gqnprg <= '1';
end gmkqwgnpqb;

entity ppiucxdlam is
  port (tjqd : buffer real; xvayftswa : in time; xs : in time; sahikwuzg : inout time);
end ppiucxdlam;

library ieee;
use ieee.std_logic_1164.all;

architecture wqdjrn of ppiucxdlam is
  signal mevmxekig : real_vector(2 to 0);
  signal ffoxc : real_vector(2 to 0);
  signal xlcdw : std_logic;
  signal b : real_vector(2 to 0);
  signal op : severity_level;
  signal yo : bit;
begin
  ilprzf : entity work.toqpvu
    port map (jwcauta => yo, wcumijw => op, a => b, gqnprg => xlcdw);
  srtfga : entity work.toqpvu
    port map (jwcauta => yo, wcumijw => op, a => ffoxc, gqnprg => xlcdw);
  qixowdlp : entity work.toqpvu
    port map (jwcauta => yo, wcumijw => op, a => mevmxekig, gqnprg => xlcdw);
  
  -- Single-driven assignments
  ffoxc <= (others => 0.0);
  tjqd <= 0_1.10001;
  op <= WARNING;
  sahikwuzg <= 16#62D.9_5_1_9_D# fs;
  yo <= '1';
  
  -- Multi-driven assignments
  xlcdw <= '-';
end wqdjrn;

entity vh is
  port (hclzjhc : in real; ppq : inout severity_level);
end vh;

library ieee;
use ieee.std_logic_1164.all;

architecture keeiype of vh is
  signal vrrxbeucq : std_logic;
  signal bicmvcn : real_vector(2 to 0);
  signal wslxfld : bit;
begin
  cdvxwp : entity work.toqpvu
    port map (jwcauta => wslxfld, wcumijw => ppq, a => bicmvcn, gqnprg => vrrxbeucq);
  
  -- Single-driven assignments
  ppq <= ERROR;
  wslxfld <= '0';
end keeiype;



-- Seed after: 6961949008355351723,17924494779688682807

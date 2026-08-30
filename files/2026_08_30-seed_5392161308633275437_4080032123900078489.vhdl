-- Seed: 5392161308633275437,4080032123900078489

library ieee;
use ieee.std_logic_1164.all;

entity bqhdjxyj is
  port (qv : inout time; ef : out std_logic_vector(1 to 2));
end bqhdjxyj;

architecture kcvxe of bqhdjxyj is
  
begin
  -- Single-driven assignments
  qv <= 8#42# ms;
  
  -- Multi-driven assignments
  ef <= ef;
  ef <= ef;
  ef <= ef;
  ef <= "LH";
end kcvxe;

library ieee;
use ieee.std_logic_1164.all;

entity zhfj is
  port (qrccpckr : out integer_vector(0 to 1); ls : linkage std_logic);
end zhfj;

library ieee;
use ieee.std_logic_1164.all;

architecture hmkpjnbzay of zhfj is
  signal uddmwnzi : std_logic_vector(1 to 2);
  signal y : time;
  signal lef : time;
  signal n : std_logic_vector(1 to 2);
  signal perbaeub : time;
  signal s : std_logic_vector(1 to 2);
  signal vwx : time;
begin
  ayhtirk : entity work.bqhdjxyj
    port map (qv => vwx, ef => s);
  epqpz : entity work.bqhdjxyj
    port map (qv => perbaeub, ef => n);
  ktoj : entity work.bqhdjxyj
    port map (qv => lef, ef => s);
  vb : entity work.bqhdjxyj
    port map (qv => y, ef => uddmwnzi);
  
  -- Single-driven assignments
  qrccpckr <= (2_3_3, 16#0_7#);
  
  -- Multi-driven assignments
  uddmwnzi <= s;
  n <= s;
end hmkpjnbzay;



-- Seed after: 7292731458719333190,4080032123900078489

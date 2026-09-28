-- Seed: 9428727089095162862,7311216359267151659

library ieee;
use ieee.std_logic_1164.all;

entity yyaeswbff is
  port (eodqzw : out real; vfa : linkage std_logic);
end yyaeswbff;

architecture hcdhlvkzig of yyaeswbff is
  
begin
  -- Single-driven assignments
  eodqzw <= 4_1.3_0_3;
end hcdhlvkzig;

entity b is
  port (zh : buffer real; csnz : in real_vector(2 to 3));
end b;

library ieee;
use ieee.std_logic_1164.all;

architecture i of b is
  signal dzdqmbs : std_logic;
  signal ezsvdhvpr : real;
begin
  mx : entity work.yyaeswbff
    port map (eodqzw => ezsvdhvpr, vfa => dzdqmbs);
  pczpcm : entity work.yyaeswbff
    port map (eodqzw => zh, vfa => dzdqmbs);
end i;

entity hnavuecqms is
  port (bzfuu : in integer; uscdd : buffer integer);
end hnavuecqms;

library ieee;
use ieee.std_logic_1164.all;

architecture jcwax of hnavuecqms is
  signal kg : real_vector(2 to 3);
  signal miyhfhgxqu : real;
  signal ebunbljw : real;
  signal e : real;
  signal kvcftppaxt : std_logic;
  signal m : real;
begin
  ryll : entity work.yyaeswbff
    port map (eodqzw => m, vfa => kvcftppaxt);
  zzttuwz : entity work.yyaeswbff
    port map (eodqzw => e, vfa => kvcftppaxt);
  aqiwev : entity work.yyaeswbff
    port map (eodqzw => ebunbljw, vfa => kvcftppaxt);
  xyllpa : entity work.b
    port map (zh => miyhfhgxqu, csnz => kg);
  
  -- Single-driven assignments
  uscdd <= 2#1111#;
  kg <= (4.0_0_1, 8#6.2#);
  
  -- Multi-driven assignments
  kvcftppaxt <= kvcftppaxt;
end jcwax;



-- Seed after: 8115999931598345302,7311216359267151659

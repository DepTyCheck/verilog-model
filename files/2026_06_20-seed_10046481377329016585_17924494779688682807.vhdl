-- Seed: 10046481377329016585,17924494779688682807

entity rzwbpl is
  port (kr : out severity_level; bhawjn : out boolean; xtluxwe : out real_vector(4 to 2));
end rzwbpl;

architecture wdzdj of rzwbpl is
  
begin
  -- Single-driven assignments
  bhawjn <= TRUE;
  xtluxwe <= (others => 0.0);
end wdzdj;

entity frwho is
  port (mc : buffer real; hqhvbuf : linkage character);
end frwho;

architecture eeakpmjo of frwho is
  signal xfldln : real_vector(4 to 2);
  signal pn : boolean;
  signal zejtbbmqr : severity_level;
begin
  wyblkvgy : entity work.rzwbpl
    port map (kr => zejtbbmqr, bhawjn => pn, xtluxwe => xfldln);
  
  -- Single-driven assignments
  mc <= 16#E0.A_0#;
end eeakpmjo;

library ieee;
use ieee.std_logic_1164.all;

entity mvjabd is
  port (wbuqwbhi : in std_logic_vector(3 to 1); hae : inout boolean);
end mvjabd;

architecture bjujqyuu of mvjabd is
  signal gsa : character;
  signal u : real;
  signal lh : real_vector(4 to 2);
  signal rygcec : boolean;
  signal wjqhov : severity_level;
  signal ezejgy : real_vector(4 to 2);
  signal jxxzqovjck : severity_level;
  signal aqmxqhny : real_vector(4 to 2);
  signal kfr : boolean;
  signal igl : severity_level;
begin
  jl : entity work.rzwbpl
    port map (kr => igl, bhawjn => kfr, xtluxwe => aqmxqhny);
  gzxpjyt : entity work.rzwbpl
    port map (kr => jxxzqovjck, bhawjn => hae, xtluxwe => ezejgy);
  vl : entity work.rzwbpl
    port map (kr => wjqhov, bhawjn => rygcec, xtluxwe => lh);
  gjzbjapw : entity work.frwho
    port map (mc => u, hqhvbuf => gsa);
end bjujqyuu;

library ieee;
use ieee.std_logic_1164.all;

entity adxedg is
  port (shrbpi : in bit; wtkpz : out real; uhyxt : inout std_logic_vector(0 to 1); jwjduetoeh : out real_vector(4 to 0));
end adxedg;

architecture jfjscb of adxedg is
  signal d : boolean;
  signal fukgkcxmi : severity_level;
begin
  hnyyrkfv : entity work.rzwbpl
    port map (kr => fukgkcxmi, bhawjn => d, xtluxwe => jwjduetoeh);
  
  -- Multi-driven assignments
  uhyxt <= ('X', '0');
  uhyxt <= ('U', 'L');
  uhyxt <= "HZ";
end jfjscb;



-- Seed after: 12264983773883424674,17924494779688682807

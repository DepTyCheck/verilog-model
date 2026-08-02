-- Seed: 10831512923744643439,13592003931158285879

library ieee;
use ieee.std_logic_1164.all;

entity kofvcffh is
  port (qmbjfxdl : in std_logic; eeduw : buffer real);
end kofvcffh;

architecture kkx of kofvcffh is
  
begin
  -- Single-driven assignments
  eeduw <= 8#0632.1_4_5#;
end kkx;

entity ozyofpmpq is
  port (dtijmpdroq : inout real; ilabgzqlk : buffer real; ta : in severity_level);
end ozyofpmpq;

library ieee;
use ieee.std_logic_1164.all;

architecture bcp of ozyofpmpq is
  signal fjl : std_logic;
  signal mzgkwilk : real;
  signal bnjl : std_logic;
begin
  vkqkqipnq : entity work.kofvcffh
    port map (qmbjfxdl => bnjl, eeduw => ilabgzqlk);
  uledyizm : entity work.kofvcffh
    port map (qmbjfxdl => bnjl, eeduw => mzgkwilk);
  tirttmjn : entity work.kofvcffh
    port map (qmbjfxdl => fjl, eeduw => dtijmpdroq);
  
  -- Multi-driven assignments
  fjl <= bnjl;
end bcp;

library ieee;
use ieee.std_logic_1164.all;

entity zf is
  port (wagstp : in std_logic_vector(1 downto 2));
end zf;

library ieee;
use ieee.std_logic_1164.all;

architecture dspgycms of zf is
  signal wppa : real;
  signal udiesngxo : std_logic;
  signal wyklrqjwqb : real;
  signal hoi : std_logic;
begin
  fjc : entity work.kofvcffh
    port map (qmbjfxdl => hoi, eeduw => wyklrqjwqb);
  ytknyx : entity work.kofvcffh
    port map (qmbjfxdl => udiesngxo, eeduw => wppa);
end dspgycms;



-- Seed after: 13932275005652033147,13592003931158285879

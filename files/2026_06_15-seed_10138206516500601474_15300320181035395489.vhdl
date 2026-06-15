-- Seed: 10138206516500601474,15300320181035395489

library ieee;
use ieee.std_logic_1164.all;

entity fdxarcuae is
  port (qiuemlqb : out std_logic_vector(3 to 0));
end fdxarcuae;

architecture nzhrpuf of fdxarcuae is
  
begin
  -- Multi-driven assignments
  qiuemlqb <= "";
  qiuemlqb <= (others => '0');
  qiuemlqb <= "";
end nzhrpuf;

entity dlwpav is
  port (klb : buffer severity_level; gginislqg : buffer real; g : inout real);
end dlwpav;

library ieee;
use ieee.std_logic_1164.all;

architecture xurcpysrw of dlwpav is
  signal kzhwc : std_logic_vector(3 to 0);
  signal jhike : std_logic_vector(3 to 0);
begin
  io : entity work.fdxarcuae
    port map (qiuemlqb => jhike);
  dhndilyr : entity work.fdxarcuae
    port map (qiuemlqb => jhike);
  wybzm : entity work.fdxarcuae
    port map (qiuemlqb => kzhwc);
  
  -- Single-driven assignments
  g <= 16#6.E8#;
  klb <= ERROR;
  gginislqg <= 16#27D.4#;
  
  -- Multi-driven assignments
  jhike <= "";
  jhike <= "";
end xurcpysrw;



-- Seed after: 12043698005614259694,15300320181035395489

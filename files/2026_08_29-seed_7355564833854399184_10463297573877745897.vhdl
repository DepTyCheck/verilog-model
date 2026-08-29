-- Seed: 7355564833854399184,10463297573877745897

library ieee;
use ieee.std_logic_1164.all;

entity ywyv is
  port (dvj : in time; zhpul : in real; s : linkage std_logic_vector(3 downto 1));
end ywyv;

architecture nlevqpoqsq of ywyv is
  
begin
  
end nlevqpoqsq;

entity cypgo is
  port (wdrfpfgy : buffer time; g : out character);
end cypgo;

library ieee;
use ieee.std_logic_1164.all;

architecture bpa of cypgo is
  signal fnfqfvcw : std_logic_vector(3 downto 1);
  signal llxkvk : real;
begin
  odwfsicnuq : entity work.ywyv
    port map (dvj => wdrfpfgy, zhpul => llxkvk, s => fnfqfvcw);
  
  -- Multi-driven assignments
  fnfqfvcw <= ('L', 'U', '1');
  fnfqfvcw <= fnfqfvcw;
  fnfqfvcw <= fnfqfvcw;
end bpa;

library ieee;
use ieee.std_logic_1164.all;

entity rtfj is
  port (touqura : buffer std_logic_vector(4 downto 4));
end rtfj;

architecture vfkwxjy of rtfj is
  
begin
  -- Multi-driven assignments
  touqura <= touqura;
  touqura <= "-";
end vfkwxjy;

entity d is
  port (ylndxkzzz : buffer integer_vector(4 to 4));
end d;

library ieee;
use ieee.std_logic_1164.all;

architecture rjzcno of d is
  signal zbbbibq : character;
  signal dqdsz : time;
  signal eenzt : std_logic_vector(4 downto 4);
begin
  b : entity work.rtfj
    port map (touqura => eenzt);
  jeis : entity work.rtfj
    port map (touqura => eenzt);
  uyidpm : entity work.cypgo
    port map (wdrfpfgy => dqdsz, g => zbbbibq);
  
  -- Single-driven assignments
  ylndxkzzz <= ylndxkzzz;
  
  -- Multi-driven assignments
  eenzt <= "H";
  eenzt <= (others => '-');
  eenzt <= eenzt;
  eenzt <= eenzt;
end rjzcno;



-- Seed after: 498261983421806703,10463297573877745897

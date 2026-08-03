-- Seed: 6567556728185763199,12359743974512393525

library ieee;
use ieee.std_logic_1164.all;

entity f is
  port (hoxvg : out std_logic_vector(3 downto 3));
end f;

architecture aakrsdls of f is
  
begin
  -- Multi-driven assignments
  hoxvg <= (others => 'U');
  hoxvg <= hoxvg;
end aakrsdls;

library ieee;
use ieee.std_logic_1164.all;

entity lgq is
  port (jmlwidqb : buffer std_logic_vector(1 to 3); vzdmnbggwk : in string(2 to 3); grdhgv : out real);
end lgq;

library ieee;
use ieee.std_logic_1164.all;

architecture j of lgq is
  signal uiqlg : std_logic_vector(3 downto 3);
  signal zqexwzqg : std_logic_vector(3 downto 3);
  signal o : std_logic_vector(3 downto 3);
begin
  ms : entity work.f
    port map (hoxvg => o);
  lnjprw : entity work.f
    port map (hoxvg => o);
  msudzpyytp : entity work.f
    port map (hoxvg => zqexwzqg);
  hmzvec : entity work.f
    port map (hoxvg => uiqlg);
  
  -- Single-driven assignments
  grdhgv <= 8#00.035#;
  
  -- Multi-driven assignments
  jmlwidqb <= "UH1";
end j;



-- Seed after: 8337815608511892515,12359743974512393525

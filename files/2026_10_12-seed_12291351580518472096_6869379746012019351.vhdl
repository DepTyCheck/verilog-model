-- Seed: 12291351580518472096,6869379746012019351

entity xzalxholi is
  port (nonmhkvbcc : buffer boolean);
end xzalxholi;

architecture mw of xzalxholi is
  
begin
  -- Single-driven assignments
  nonmhkvbcc <= nonmhkvbcc;
end mw;

library ieee;
use ieee.std_logic_1164.all;

entity nbnlxuwoxe is
  port ( cvythx : buffer std_logic_vector(0 downto 4)
  ; tpdchx : out boolean
  ; uydtpce : buffer std_logic_vector(3 to 1)
  ; swlrguoz : linkage std_logic_vector(1 downto 0)
  );
end nbnlxuwoxe;

architecture kim of nbnlxuwoxe is
  signal zyhqpjo : boolean;
begin
  i : entity work.xzalxholi
    port map (nonmhkvbcc => tpdchx);
  uaabhew : entity work.xzalxholi
    port map (nonmhkvbcc => zyhqpjo);
  
  -- Multi-driven assignments
  cvythx <= (others => '0');
end kim;

library ieee;
use ieee.std_logic_1164.all;

entity oh is
  port (sv : buffer bit_vector(4 downto 4); ppapnconx : inout time; urts : inout std_logic);
end oh;

library ieee;
use ieee.std_logic_1164.all;

architecture mahg of oh is
  signal bbtpbxxsbc : boolean;
  signal ayofeuvfmn : std_logic_vector(1 downto 0);
  signal odiuovwxgd : boolean;
  signal apmwbffk : std_logic_vector(3 to 1);
begin
  gn : entity work.nbnlxuwoxe
    port map (cvythx => apmwbffk, tpdchx => odiuovwxgd, uydtpce => apmwbffk, swlrguoz => ayofeuvfmn);
  m : entity work.xzalxholi
    port map (nonmhkvbcc => bbtpbxxsbc);
  
  -- Single-driven assignments
  ppapnconx <= ppapnconx;
  sv <= (others => '0');
  
  -- Multi-driven assignments
  apmwbffk <= apmwbffk;
  urts <= 'H';
  ayofeuvfmn <= "1-";
  urts <= 'Z';
end mahg;

entity acxee is
  port (acvxuulp : buffer integer; nmrjobf : buffer time);
end acxee;

library ieee;
use ieee.std_logic_1164.all;

architecture mbx of acxee is
  signal kec : boolean;
  signal jlk : std_logic_vector(3 to 1);
  signal fskxqh : boolean;
  signal hfw : std_logic_vector(1 downto 0);
  signal pherfd : std_logic_vector(3 to 1);
  signal p : boolean;
  signal bzvqlzazl : std_logic_vector(0 downto 4);
  signal rgurbw : boolean;
begin
  evphagsq : entity work.xzalxholi
    port map (nonmhkvbcc => rgurbw);
  tjb : entity work.nbnlxuwoxe
    port map (cvythx => bzvqlzazl, tpdchx => p, uydtpce => pherfd, swlrguoz => hfw);
  zjkw : entity work.nbnlxuwoxe
    port map (cvythx => bzvqlzazl, tpdchx => fskxqh, uydtpce => jlk, swlrguoz => hfw);
  u : entity work.xzalxholi
    port map (nonmhkvbcc => kec);
end mbx;



-- Seed after: 6691652497946561928,6869379746012019351

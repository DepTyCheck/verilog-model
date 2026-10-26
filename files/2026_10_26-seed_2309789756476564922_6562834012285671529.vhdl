-- Seed: 2309789756476564922,6562834012285671529

library ieee;
use ieee.std_logic_1164.all;

entity kyvgzo is
  port (pnyzbucl : linkage std_logic_vector(1 to 0));
end kyvgzo;

architecture skpzsq of kyvgzo is
  
begin
  
end skpzsq;

library ieee;
use ieee.std_logic_1164.all;

entity ozif is
  port (ppahfm : in std_logic; z : buffer std_logic_vector(1 to 2));
end ozif;

library ieee;
use ieee.std_logic_1164.all;

architecture uprupcw of ozif is
  signal opwse : std_logic_vector(1 to 0);
  signal vuhztx : std_logic_vector(1 to 0);
begin
  vs : entity work.kyvgzo
    port map (pnyzbucl => vuhztx);
  bhdv : entity work.kyvgzo
    port map (pnyzbucl => vuhztx);
  ltjhiokv : entity work.kyvgzo
    port map (pnyzbucl => opwse);
  oiynxy : entity work.kyvgzo
    port map (pnyzbucl => vuhztx);
  
  -- Multi-driven assignments
  z <= ('X', 'W');
  opwse <= vuhztx;
end uprupcw;

entity djvotj is
  port (daffz : inout real; bbtt : inout integer);
end djvotj;

library ieee;
use ieee.std_logic_1164.all;

architecture errerle of djvotj is
  signal ltheyvnuqu : std_logic_vector(1 to 0);
  signal glumj : std_logic_vector(1 to 2);
  signal ctxrzo : std_logic;
begin
  irllbgfgd : entity work.ozif
    port map (ppahfm => ctxrzo, z => glumj);
  hgek : entity work.ozif
    port map (ppahfm => ctxrzo, z => glumj);
  iwuacv : entity work.kyvgzo
    port map (pnyzbucl => ltheyvnuqu);
  kczi : entity work.ozif
    port map (ppahfm => ctxrzo, z => glumj);
  
  -- Single-driven assignments
  bbtt <= 8#6_0#;
  daffz <= 41414.3;
end errerle;



-- Seed after: 8242634867000792021,6562834012285671529

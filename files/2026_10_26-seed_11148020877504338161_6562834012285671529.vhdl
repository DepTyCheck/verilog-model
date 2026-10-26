-- Seed: 11148020877504338161,6562834012285671529

library ieee;
use ieee.std_logic_1164.all;

entity bp is
  port (octremd : in character; rdxinpng : linkage string(5 downto 1); dy : linkage real; zdgwczve : in std_logic);
end bp;

architecture g of bp is
  
begin
  
end g;

library ieee;
use ieee.std_logic_1164.all;

entity oecgcjb is
  port (fgmqdtrv : linkage std_logic; hp : in real);
end oecgcjb;

library ieee;
use ieee.std_logic_1164.all;

architecture l of oecgcjb is
  signal xggccznez : real;
  signal onxw : string(5 downto 1);
  signal da : character;
  signal f : real;
  signal iaty : string(5 downto 1);
  signal cvdmbrrzl : character;
  signal tsd : std_logic;
  signal gtccsrv : real;
  signal zd : string(5 downto 1);
  signal g : character;
begin
  ou : entity work.bp
    port map (octremd => g, rdxinpng => zd, dy => gtccsrv, zdgwczve => tsd);
  vpuw : entity work.bp
    port map (octremd => cvdmbrrzl, rdxinpng => iaty, dy => f, zdgwczve => tsd);
  xcheumk : entity work.bp
    port map (octremd => da, rdxinpng => onxw, dy => xggccznez, zdgwczve => tsd);
  
  -- Single-driven assignments
  g <= 'm';
  da <= 'p';
end l;

library ieee;
use ieee.std_logic_1164.all;

entity bjun is
  port (piyafa : buffer std_logic; dvrosr : buffer std_logic; mosxk : in bit; lizfs : out real);
end bjun;

library ieee;
use ieee.std_logic_1164.all;

architecture dqofycytz of bjun is
  signal dtncbn : std_logic;
begin
  koukuju : entity work.oecgcjb
    port map (fgmqdtrv => dtncbn, hp => lizfs);
  
  -- Single-driven assignments
  lizfs <= 8#4600.743#;
  
  -- Multi-driven assignments
  dvrosr <= dvrosr;
end dqofycytz;



-- Seed after: 16203565322342317411,6562834012285671529

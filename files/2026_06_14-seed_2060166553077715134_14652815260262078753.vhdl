-- Seed: 2060166553077715134,14652815260262078753

library ieee;
use ieee.std_logic_1164.all;

entity om is
  port (hovfezum : in std_logic_vector(1 to 3); fblocxmkuq : out real_vector(2 to 1); piyryafowi : out std_logic_vector(3 downto 4));
end om;

architecture mpacnoo of om is
  
begin
  -- Single-driven assignments
  fblocxmkuq <= (others => 0.0);
end mpacnoo;

library ieee;
use ieee.std_logic_1164.all;

entity wyixuk is
  port (ylsactlrh : in time; yevttultw : inout std_logic_vector(0 to 1));
end wyixuk;

library ieee;
use ieee.std_logic_1164.all;

architecture uqeyoks of wyixuk is
  signal lhzokyo : real_vector(2 to 1);
  signal vypogibffa : std_logic_vector(1 to 3);
  signal tupimrazro : std_logic_vector(3 downto 4);
  signal sfxr : real_vector(2 to 1);
  signal fn : std_logic_vector(3 downto 4);
  signal dtxissiaa : real_vector(2 to 1);
  signal nx : std_logic_vector(1 to 3);
begin
  wtig : entity work.om
    port map (hovfezum => nx, fblocxmkuq => dtxissiaa, piyryafowi => fn);
  mzxnmpom : entity work.om
    port map (hovfezum => nx, fblocxmkuq => sfxr, piyryafowi => tupimrazro);
  zkyd : entity work.om
    port map (hovfezum => vypogibffa, fblocxmkuq => lhzokyo, piyryafowi => fn);
  
  -- Multi-driven assignments
  tupimrazro <= (others => '0');
  yevttultw <= "1-";
  tupimrazro <= "";
  vypogibffa <= ('L', '0', '-');
end uqeyoks;

entity ewjqgx is
  port (w : in character);
end ewjqgx;

library ieee;
use ieee.std_logic_1164.all;

architecture pwyftffktw of ewjqgx is
  signal jzwo : std_logic_vector(3 downto 4);
  signal nopoonet : real_vector(2 to 1);
  signal smtzbuezlr : std_logic_vector(1 to 3);
begin
  pa : entity work.om
    port map (hovfezum => smtzbuezlr, fblocxmkuq => nopoonet, piyryafowi => jzwo);
  
  -- Multi-driven assignments
  jzwo <= "";
  jzwo <= (others => '0');
  smtzbuezlr <= "0L0";
end pwyftffktw;



-- Seed after: 6649906953216528061,14652815260262078753

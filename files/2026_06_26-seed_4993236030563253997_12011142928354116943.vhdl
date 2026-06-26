-- Seed: 4993236030563253997,12011142928354116943

library ieee;
use ieee.std_logic_1164.all;

entity ejfitg is
  port (joavmocsg : buffer std_logic_vector(3 downto 2); fa : in integer_vector(4 downto 1));
end ejfitg;

architecture e of ejfitg is
  
begin
  -- Multi-driven assignments
  joavmocsg <= ('1', 'Z');
end e;

entity jcsprcc is
  port (we : in integer);
end jcsprcc;

library ieee;
use ieee.std_logic_1164.all;

architecture lxc of jcsprcc is
  signal lkzddekwv : integer_vector(4 downto 1);
  signal soo : integer_vector(4 downto 1);
  signal nvtio : std_logic_vector(3 downto 2);
  signal crdfhon : std_logic_vector(3 downto 2);
  signal t : integer_vector(4 downto 1);
  signal rlnsdqu : std_logic_vector(3 downto 2);
begin
  avsqmtkbh : entity work.ejfitg
    port map (joavmocsg => rlnsdqu, fa => t);
  yraw : entity work.ejfitg
    port map (joavmocsg => crdfhon, fa => t);
  msq : entity work.ejfitg
    port map (joavmocsg => nvtio, fa => soo);
  zhjvmjhf : entity work.ejfitg
    port map (joavmocsg => rlnsdqu, fa => lkzddekwv);
  
  -- Multi-driven assignments
  rlnsdqu <= ('H', 'U');
  rlnsdqu <= "UH";
  rlnsdqu <= "0-";
end lxc;

library ieee;
use ieee.std_logic_1164.all;

entity towsivac is
  port (wjucaxpy : inout std_logic_vector(0 downto 3); ysb : linkage character);
end towsivac;

library ieee;
use ieee.std_logic_1164.all;

architecture a of towsivac is
  signal svasrvyrza : integer_vector(4 downto 1);
  signal qaeyr : std_logic_vector(3 downto 2);
  signal ty : integer_vector(4 downto 1);
  signal gjwgfasdk : std_logic_vector(3 downto 2);
  signal v : integer;
begin
  un : entity work.jcsprcc
    port map (we => v);
  zcqzwdfkt : entity work.ejfitg
    port map (joavmocsg => gjwgfasdk, fa => ty);
  qqjwd : entity work.jcsprcc
    port map (we => v);
  vlkdcjx : entity work.ejfitg
    port map (joavmocsg => qaeyr, fa => svasrvyrza);
  
  -- Single-driven assignments
  v <= 2#0#;
  ty <= (2#01011#, 4401, 3_1_1_0_1, 8#17316#);
  
  -- Multi-driven assignments
  wjucaxpy <= "";
  wjucaxpy <= "";
  gjwgfasdk <= "X0";
  gjwgfasdk <= "XU";
end a;



-- Seed after: 15753889555158542110,12011142928354116943

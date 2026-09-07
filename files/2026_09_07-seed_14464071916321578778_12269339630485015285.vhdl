-- Seed: 14464071916321578778,12269339630485015285

entity s is
  port (mbnbebbfyh : out time);
end s;

architecture m of s is
  
begin
  -- Single-driven assignments
  mbnbebbfyh <= 0_0_0_1.0 fs;
end m;

entity ve is
  port (pishxtxpj : inout time_vector(0 downto 2); akid : out character; bmpt : buffer severity_level);
end ve;

architecture zmffaa of ve is
  signal wiytq : time;
  signal mxkc : time;
  signal efoubtblvm : time;
begin
  pirpziv : entity work.s
    port map (mbnbebbfyh => efoubtblvm);
  uhzupplq : entity work.s
    port map (mbnbebbfyh => mxkc);
  attluxh : entity work.s
    port map (mbnbebbfyh => wiytq);
  
  -- Single-driven assignments
  bmpt <= bmpt;
  pishxtxpj <= pishxtxpj;
  akid <= 'a';
end zmffaa;

library ieee;
use ieee.std_logic_1164.all;

entity wj is
  port ( hj : in std_logic_vector(3 downto 0)
  ; gqrbwgtrk : in integer
  ; tynieris : out std_logic_vector(4 to 3)
  ; vf : buffer std_logic_vector(0 downto 0)
  );
end wj;

architecture htxb of wj is
  signal kropdystym : time;
  signal ipyl : time;
  signal x : time;
  signal mx : severity_level;
  signal wynwizunw : character;
  signal cfmwnxkv : time_vector(0 downto 2);
begin
  jetcrpowo : entity work.ve
    port map (pishxtxpj => cfmwnxkv, akid => wynwizunw, bmpt => mx);
  hnxauvj : entity work.s
    port map (mbnbebbfyh => x);
  hmpfp : entity work.s
    port map (mbnbebbfyh => ipyl);
  dirlk : entity work.s
    port map (mbnbebbfyh => kropdystym);
  
  -- Multi-driven assignments
  vf <= (others => '-');
  vf <= "L";
  vf <= (others => 'H');
end htxb;



-- Seed after: 230107751510535229,12269339630485015285

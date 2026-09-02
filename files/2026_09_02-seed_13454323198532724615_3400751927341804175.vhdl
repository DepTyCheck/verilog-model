-- Seed: 13454323198532724615,3400751927341804175

library ieee;
use ieee.std_logic_1164.all;

entity enjclpzwxz is
  port (ivxizvm : out std_logic; yedcvgi : in std_logic_vector(3 downto 2); mjzg : in time);
end enjclpzwxz;

architecture hursktqus of enjclpzwxz is
  
begin
  -- Multi-driven assignments
  ivxizvm <= 'L';
  ivxizvm <= ivxizvm;
end hursktqus;

library ieee;
use ieee.std_logic_1164.all;

entity f is
  port (zoigsgtcro : inout std_logic; ebtdwybui : inout std_logic_vector(3 to 1); yt : out std_logic; jxjn : out bit_vector(3 to 0));
end f;

library ieee;
use ieee.std_logic_1164.all;

architecture spncacadrz of f is
  signal dbwo : std_logic_vector(3 downto 2);
  signal eprofvttm : std_logic;
  signal mik : time;
  signal wycazfxft : time;
  signal p : std_logic_vector(3 downto 2);
  signal krp : std_logic;
begin
  bsyemjzlp : entity work.enjclpzwxz
    port map (ivxizvm => krp, yedcvgi => p, mjzg => wycazfxft);
  pbxsfp : entity work.enjclpzwxz
    port map (ivxizvm => krp, yedcvgi => p, mjzg => mik);
  oabf : entity work.enjclpzwxz
    port map (ivxizvm => eprofvttm, yedcvgi => dbwo, mjzg => wycazfxft);
  
  -- Single-driven assignments
  jxjn <= (others => '0');
  
  -- Multi-driven assignments
  yt <= yt;
  yt <= krp;
  eprofvttm <= yt;
end spncacadrz;

entity bpjncto is
  port (azl : inout integer; htlexwqzgc : buffer character);
end bpjncto;

library ieee;
use ieee.std_logic_1164.all;

architecture nrxb of bpjncto is
  signal xgvrzj : std_logic_vector(3 downto 2);
  signal spdhy : std_logic;
  signal wkv : time;
  signal sdy : std_logic_vector(3 downto 2);
  signal fvczfpug : std_logic;
  signal wzsbtvgeji : bit_vector(3 to 0);
  signal nxiilwvs : std_logic;
  signal qztrshkpaa : std_logic_vector(3 to 1);
  signal be : std_logic;
begin
  rfbjejmj : entity work.f
    port map (zoigsgtcro => be, ebtdwybui => qztrshkpaa, yt => nxiilwvs, jxjn => wzsbtvgeji);
  rxo : entity work.enjclpzwxz
    port map (ivxizvm => fvczfpug, yedcvgi => sdy, mjzg => wkv);
  ycxb : entity work.enjclpzwxz
    port map (ivxizvm => spdhy, yedcvgi => xgvrzj, mjzg => wkv);
  
  -- Single-driven assignments
  htlexwqzgc <= 'a';
  wkv <= 2#1_1_1.0# ns;
  azl <= azl;
  
  -- Multi-driven assignments
  sdy <= ('1', '1');
  fvczfpug <= 'Z';
  be <= be;
end nrxb;



-- Seed after: 842719920925077186,3400751927341804175

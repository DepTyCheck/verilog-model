-- Seed: 15736562891783041352,2511821214772927453

library ieee;
use ieee.std_logic_1164.all;

entity qisttjjovs is
  port (wg : in real_vector(4 downto 4); k : in string(4 to 2); xz : in std_logic_vector(0 to 2));
end qisttjjovs;

architecture ddgsxxkfuc of qisttjjovs is
  
begin
  
end ddgsxxkfuc;

entity kfugehnpk is
  port (whluygsrx : buffer string(1 to 4); ebeivccoo : buffer integer_vector(3 to 1));
end kfugehnpk;

library ieee;
use ieee.std_logic_1164.all;

architecture e of kfugehnpk is
  signal pvukdv : std_logic_vector(0 to 2);
  signal seltufuhbr : string(4 to 2);
  signal utpokqdiko : real_vector(4 downto 4);
  signal hcvrtqloyk : std_logic_vector(0 to 2);
  signal q : string(4 to 2);
  signal mfndhlknf : real_vector(4 downto 4);
begin
  gkdcv : entity work.qisttjjovs
    port map (wg => mfndhlknf, k => q, xz => hcvrtqloyk);
  zhwjgkbhl : entity work.qisttjjovs
    port map (wg => utpokqdiko, k => seltufuhbr, xz => pvukdv);
  
  -- Single-driven assignments
  q <= (others => ' ');
  whluygsrx <= ('u', 's', 'z', 'd');
  
  -- Multi-driven assignments
  pvukdv <= hcvrtqloyk;
  hcvrtqloyk <= hcvrtqloyk;
end e;

entity hzngmoylz is
  port (ridwhrk : out time; xxfcd : out boolean_vector(0 to 0); whxcgf : buffer real_vector(1 to 0); c : buffer real);
end hzngmoylz;

library ieee;
use ieee.std_logic_1164.all;

architecture tbz of hzngmoylz is
  signal tzrnmqvtm : std_logic_vector(0 to 2);
  signal osf : std_logic_vector(0 to 2);
  signal vqjhiswx : string(4 to 2);
  signal zrhruh : real_vector(4 downto 4);
begin
  giablze : entity work.qisttjjovs
    port map (wg => zrhruh, k => vqjhiswx, xz => osf);
  qzyn : entity work.qisttjjovs
    port map (wg => zrhruh, k => vqjhiswx, xz => tzrnmqvtm);
  
  -- Single-driven assignments
  xxfcd <= (others => FALSE);
  whxcgf <= (others => 0.0);
  ridwhrk <= 23 ns;
  c <= c;
  
  -- Multi-driven assignments
  osf <= ('Z', 'Z', 'Z');
  tzrnmqvtm <= osf;
end tbz;

entity gntrk is
  port (yyw : out real; leuadzl : in time_vector(0 downto 2));
end gntrk;

architecture dmxhs of gntrk is
  
begin
  -- Single-driven assignments
  yyw <= 2#1.1_1_1#;
end dmxhs;



-- Seed after: 17312810013903706685,2511821214772927453

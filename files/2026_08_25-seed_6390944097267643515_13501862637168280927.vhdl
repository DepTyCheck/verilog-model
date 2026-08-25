-- Seed: 6390944097267643515,13501862637168280927

entity icuks is
  port (zzhunsrib : linkage severity_level; vq : buffer real; qekfmecof : linkage time_vector(3 downto 3); rwrediaxcs : in integer);
end icuks;

architecture bphvsoyb of icuks is
  
begin
  
end bphvsoyb;

entity rm is
  port (pdeewehu : inout time);
end rm;

architecture arhrnuryi of rm is
  signal l : integer;
  signal evcspzi : time_vector(3 downto 3);
  signal yfxtlch : real;
  signal lhu : severity_level;
begin
  pmq : entity work.icuks
    port map (zzhunsrib => lhu, vq => yfxtlch, qekfmecof => evcspzi, rwrediaxcs => l);
  
  -- Single-driven assignments
  pdeewehu <= pdeewehu;
  l <= 3;
end arhrnuryi;

entity uu is
  port (yhjdqpu : inout boolean; rxd : out real);
end uu;

architecture efnq of uu is
  signal ejs : time_vector(3 downto 3);
  signal gtnefnye : severity_level;
  signal uqclhduwc : integer;
  signal cryiy : time_vector(3 downto 3);
  signal rybz : real;
  signal qn : severity_level;
begin
  rybl : entity work.icuks
    port map (zzhunsrib => qn, vq => rybz, qekfmecof => cryiy, rwrediaxcs => uqclhduwc);
  gyrv : entity work.icuks
    port map (zzhunsrib => gtnefnye, vq => rxd, qekfmecof => ejs, rwrediaxcs => uqclhduwc);
end efnq;



-- Seed after: 10109739217118665150,13501862637168280927

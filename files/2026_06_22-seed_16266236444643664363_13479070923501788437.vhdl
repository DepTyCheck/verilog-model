-- Seed: 16266236444643664363,13479070923501788437

entity ictmcbordn is
  port (slil : out integer; jsigc : linkage time_vector(3 to 1));
end ictmcbordn;

architecture ezmbq of ictmcbordn is
  
begin
  
end ezmbq;

library ieee;
use ieee.std_logic_1164.all;

entity jx is
  port (gzkhtjvv : in character; xed : linkage time; nadmo : out time; kjxqe : out std_logic_vector(2 to 4));
end jx;

architecture gwrwzehf of jx is
  signal hegpmdqw : time_vector(3 to 1);
  signal w : integer;
  signal gvo : time_vector(3 to 1);
  signal k : integer;
  signal ohfr : time_vector(3 to 1);
  signal pkobism : integer;
  signal rtpft : time_vector(3 to 1);
  signal rdcks : integer;
begin
  vi : entity work.ictmcbordn
    port map (slil => rdcks, jsigc => rtpft);
  ekzvrkuvf : entity work.ictmcbordn
    port map (slil => pkobism, jsigc => ohfr);
  hrugzzbn : entity work.ictmcbordn
    port map (slil => k, jsigc => gvo);
  dnki : entity work.ictmcbordn
    port map (slil => w, jsigc => hegpmdqw);
  
  -- Single-driven assignments
  nadmo <= 8#2473# ns;
  
  -- Multi-driven assignments
  kjxqe <= "X-Z";
  kjxqe <= "XZH";
  kjxqe <= ('L', 'W', 'Z');
  kjxqe <= ('0', 'U', 'H');
end gwrwzehf;



-- Seed after: 3868044332407870472,13479070923501788437

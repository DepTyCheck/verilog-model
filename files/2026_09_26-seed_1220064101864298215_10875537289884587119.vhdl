-- Seed: 1220064101864298215,10875537289884587119

entity j is
  port (yfm : buffer time; wjvbzpwm : in string(2 to 4));
end j;

architecture gbp of j is
  
begin
  -- Single-driven assignments
  yfm <= 8#5_7_5_7# fs;
end gbp;

entity sxrmp is
  port (llyifwhq : out severity_level);
end sxrmp;

architecture vypfdmq of sxrmp is
  signal ffpudaw : time;
  signal rjkjbi : time;
  signal vlunzlv : time;
  signal pzu : string(2 to 4);
  signal jpx : time;
begin
  ag : entity work.j
    port map (yfm => jpx, wjvbzpwm => pzu);
  zw : entity work.j
    port map (yfm => vlunzlv, wjvbzpwm => pzu);
  zozuq : entity work.j
    port map (yfm => rjkjbi, wjvbzpwm => pzu);
  mkep : entity work.j
    port map (yfm => ffpudaw, wjvbzpwm => pzu);
end vypfdmq;

entity xiobdcar is
  port (idg : buffer integer; buajequev : buffer real);
end xiobdcar;

architecture lh of xiobdcar is
  
begin
  -- Single-driven assignments
  buajequev <= 0_4.02;
  idg <= 16#A51BD#;
end lh;

entity a is
  port (uyhhwqh : out boolean_vector(3 to 0); ezshpe : inout real; pget : inout bit);
end a;

architecture eldpbpkxe of a is
  signal q : severity_level;
  signal fslkd : string(2 to 4);
  signal gwp : time;
  signal iybffwzdb : real;
  signal dilleb : integer;
  signal jhth : severity_level;
begin
  mntkjl : entity work.sxrmp
    port map (llyifwhq => jhth);
  onwj : entity work.xiobdcar
    port map (idg => dilleb, buajequev => iybffwzdb);
  wbhugdp : entity work.j
    port map (yfm => gwp, wjvbzpwm => fslkd);
  nfdiwft : entity work.sxrmp
    port map (llyifwhq => q);
  
  -- Single-driven assignments
  ezshpe <= 2#0.0_0_1#;
  fslkd <= "uyj";
  uyhhwqh <= uyhhwqh;
  pget <= '1';
end eldpbpkxe;



-- Seed after: 3889380423316006223,10875537289884587119

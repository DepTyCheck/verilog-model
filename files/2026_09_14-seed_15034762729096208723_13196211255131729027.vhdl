-- Seed: 15034762729096208723,13196211255131729027

entity bkedluyem is
  port (fwyn : in integer; kmubib : in time; jglivhxh : linkage severity_level);
end bkedluyem;

architecture ehpiv of bkedluyem is
  
begin
  
end ehpiv;

entity yqmgnvartt is
  port (fggd : out bit_vector(1 to 2));
end yqmgnvartt;

architecture iwroaedb of yqmgnvartt is
  signal iflkhomb : severity_level;
  signal lgw : severity_level;
  signal zcqkyvosl : time;
  signal mblksn : integer;
begin
  nqf : entity work.bkedluyem
    port map (fwyn => mblksn, kmubib => zcqkyvosl, jglivhxh => lgw);
  akw : entity work.bkedluyem
    port map (fwyn => mblksn, kmubib => zcqkyvosl, jglivhxh => iflkhomb);
end iwroaedb;

entity pwh is
  port (hrtnz : inout integer_vector(4 to 3); bstk : buffer real; jvskpwu : out time);
end pwh;

architecture grpa of pwh is
  signal c : bit_vector(1 to 2);
begin
  cxtoxxepof : entity work.yqmgnvartt
    port map (fggd => c);
  
  -- Single-driven assignments
  jvskpwu <= 2#1# ns;
  hrtnz <= (others => 0);
  bstk <= 2#0_1_0_0_1.1#;
end grpa;



-- Seed after: 18375248490816740424,13196211255131729027

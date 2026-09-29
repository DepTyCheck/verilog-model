-- Seed: 6994890426366581930,10940991575366938685

entity giniplxbjh is
  port (gdcdbmfh : linkage string(2 downto 3); wgqkyen : out time; wtvtiggrsq : buffer real_vector(4 to 0); oafl : in character);
end giniplxbjh;

architecture alwaubqf of giniplxbjh is
  
begin
  
end alwaubqf;

entity twbb is
  port (qock : out real; fnhrn : in real; oofjd : out real);
end twbb;

architecture xljvndbe of twbb is
  signal fmpovp : real_vector(4 to 0);
  signal dd : time;
  signal czepgewod : string(2 downto 3);
  signal kowpnrin : real_vector(4 to 0);
  signal oyj : time;
  signal y : string(2 downto 3);
  signal zbmzl : character;
  signal qgizhpdgb : real_vector(4 to 0);
  signal vvhdewpnk : time;
  signal k : string(2 downto 3);
begin
  nqefvxt : entity work.giniplxbjh
    port map (gdcdbmfh => k, wgqkyen => vvhdewpnk, wtvtiggrsq => qgizhpdgb, oafl => zbmzl);
  my : entity work.giniplxbjh
    port map (gdcdbmfh => y, wgqkyen => oyj, wtvtiggrsq => kowpnrin, oafl => zbmzl);
  n : entity work.giniplxbjh
    port map (gdcdbmfh => czepgewod, wgqkyen => dd, wtvtiggrsq => fmpovp, oafl => zbmzl);
  
  -- Single-driven assignments
  oofjd <= fnhrn;
  qock <= 8#6_2_2.7#;
end xljvndbe;



-- Seed after: 17001193914576436706,10940991575366938685

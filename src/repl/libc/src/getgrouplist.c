#include <grp.h>
#include <stdlib.h>
#include <stdio.h>

gid_t* groups;
int ngroups;
int i;

int getgroups(char* username, int primary_group, int number_of_groups) {
  int r;
  i = 0;
  groups = malloc(number_of_groups * sizeof(gid_t));
  r = getgrouplist(username, primary_group, groups, &number_of_groups);
  ngroups = number_of_groups;
  return r;
}

int get_number_of_groups(void) {
  return ngroups;
}

int get_next_group_id(void) {
  if (i>=ngroups) return -1;
  gid_t j = groups[i];
  i++;
  return j;
}

int main()
{
  int r = getgroups("pi", 1000, 0);
  printf("Number of groups: %d\n", ngroups);
  return 0;
}
